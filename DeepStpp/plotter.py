import torch
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import plotly.graph_objects as go
from tqdm.auto import tqdm
import matplotlib.pyplot as plt
import numpy as np
import contextily as ctx
from mpl_toolkits.mplot3d import Axes3D
from pyproj import Transformer
import smopy

"""
outputs: [batch, lookahead, 3] or [batch, 3]
targets: [batch, lookahead, 3] or [batch, 3]
"""
def visualize_diff(outputs, targets, portion=1):
    if len(targets.shape) == 2:
        outputs = np.expand_dims(outputs, 1)
        targets = np.expand_dims(targets, 1)

    outputs = outputs[:int(len(outputs) * portion)]
    targets = targets[:int(len(targets) * portion)]

    plt.figure(figsize=(14, 10), dpi=180)
    plt.subplot(2, 2, 1)

    n = outputs.shape[0]
    lookahead = outputs.shape[1]

    for i in range(lookahead):
        plt.plot(range(i, n), outputs[:n - i, i, 0], "-o", label=f"Predicted {i} step")
    plt.plot(targets[:, 0, 0], "-o", color="b", label="Actual")
    plt.ylabel('Latitude')
    plt.legend()

    plt.subplot(2, 2, 2)
    for i in range(lookahead):
        plt.plot(range(i, n), outputs[:n - i, i, 1], "-o", label=f"Predicted {i} step")
    plt.plot(targets[:, 0, 1], "-o", color="b", label="Actual")
    plt.ylabel('Longitude')
    plt.legend()

    plt.subplot(2, 2, 3)
    for i in range(lookahead):
        plt.plot(range(i, n), outputs[:n - i, i, 2], "-o", label=f"Predicted {i} step")
    plt.plot(targets[:, 0, 2], "-o", color="b", label="Actual")
    plt.ylabel('delta_t (hours)')
    plt.legend()
    plt.savefig('result.png')
    
    
def frame_args(duration):
    return {
               "frame": {"duration": duration},
               "mode": "immediate",
               "fromcurrent": True,
               "transition": {"duration": duration},
           }


"""
x_range, y_range, t_range: 1D array of any length
"""
def inverse_transform(x_range, y_range, t_range, scaler):
    # Inverse transform the data
    temp = np.zeros((len(x_range), 3)) 
    temp[:, 0] = x_range
    x_range = scaler.inverse_transform(temp)[:, 0]

    temp = np.zeros((len(y_range), 3)) 
    temp[:, 1] = y_range
    y_range = scaler.inverse_transform(temp)[:, 1]

    temp = np.zeros((len(t_range), 3)) 
    temp[:, 2] = t_range
    t_range = scaler.inverse_transform(temp)[:, 2]
    
    return x_range, y_range, t_range


"""
lambs: list, len(lambs) = len(t_range), element: [len(x_range), len(y_range)]
fps: # frame per sec
fn: file_name

The result could be saved as file with command `ani.save('file_name.mp4', writer='ffmpeg', fps=fps)`
                                    or command `ani.save('file_name.gif', writer='imagemagick', fps=fps)`
"""
def plot_lambst_static(lambs, x_range, y_range, t_range, fps, scaler=None, cmin=None, cmax=None, 
                       history=None, decay=0.3, base_size=300, cmap='magma', fn='result.mp4',draw_base_map=False):
    # Inverse transform the range to the actual scale
    if scaler is not None:
        x_range, y_range, t_range = inverse_transform(x_range, y_range, t_range, scaler)
    if cmin is None:
        cmin = 0
    if cmax is "outlier":
        cmax = np.max([np.max(lamb_st) for lamb_st in lambs])
    if cmax is None:
        cmax = np.max(lambs)
    print(f'Inferred cmax: {cmax}')
    cmid = cmin + (cmax - cmin) * 0.9
        
    grid_x, grid_y = np.meshgrid(x_range, y_range)
    # Automatically calculate the bounds from the meshgrid
    lon_min, lon_max = grid_x.min(), grid_x.max()  # Longitude range
    lat_min, lat_max = grid_y.min(), grid_y.max()  # Latitude range

    frn = len(t_range) # frame number of the animation
    
    # Transform coordinates to Web Mercator (EPSG:3857)
    transformer = Transformer.from_crs("EPSG:4326", "EPSG:3857", always_xy=True)
    xmin, ymin = transformer.transform(lon_min, lat_min)  # Bottom-left corner
    xmax, ymax = transformer.transform(lon_max, lat_max)  # Top-right corner


    fig = plt.figure(figsize=(6,6), dpi=150)
    ax = fig.add_subplot(111, projection='3d', xlabel='x', ylabel='y', zlabel='λ', zlim=(cmin, cmax), 
                         title='Spatio-temporal Conditional Intensity')
    ax.title.set_position([.5, .95])
    text = ax.text(min(x_range), min(y_range), cmax, "t={:.2f}".format(t_range[0]), fontsize=10)
    plot = [ax.plot_surface(grid_x, grid_y, lambs[0], rstride=1, cstride=1, cmap=cmap)]
    
    if history is not None: 
        his_s, his_t = history
        zs = np.ones_like(lambs[0]) * cmid # add a platform for locations
        plat = ax.plot_surface(grid_x, grid_y, zs, rstride=1, cstride=1, color='white', alpha=0.2)
        points = ax.scatter3D([], [], [], color='black') # add locations 
        plot.append(plat)
        plot.append(points)
       
    if draw_base_map:
        # Fetch the basemap using calculated bounds
        basemap_img, extent = ctx.bounds2img(xmin, ymin, xmax, ymax, zoom=12, source=ctx.providers.OpenStreetMap.Mapnik)
        
        # Map the basemap as a 3D surface
        texture = basemap_img[::-1, :, :3] / 255.0  # Normalize the RGB values
        map_z = np.zeros_like(grid_x)  # Map at z=0 (base level)
        ax.plot_surface(grid_x, grid_y, map_z, rstride=1, cstride=1, facecolors=texture, shade=False, zorder=0)
    
    pbar = tqdm(total=frn + 2)
    
    def update_plot(frame_number):
        t = t_range[frame_number]
        plot[0].remove()
        plot[0] = ax.plot_surface(grid_x, grid_y, lambs[frame_number], rstride=1, cstride=1, cmap=cmap)
        text.set_text('t={:.2f}'.format(t))
        
        if history is not None:
            mask = np.logical_and(his_t <= t, his_t >= t_range[0])
            locs = his_s[mask]
            times = his_t[mask]
            sizes = np.exp((times - t) * decay) * base_size
            zs = np.ones_like(sizes) * cmid
            plot[2].remove()
            plot[2] = ax.scatter3D(locs[:, 0], locs[:, 1], zs, c='black', s=sizes, marker='x')
        
        pbar.update()
    
    ani = animation.FuncAnimation(fig, update_plot, frn, interval=1000/fps)
    ani.save(fn, writer='ffmpeg', fps=fps)
    return ani
    
    
"""
lambs: list, len(lambs) = len(t_range), element: [len(x_range), len(y_range)]
"""
def plot_lambst_interactive(lambs, x_range, y_range, t_range, cmin=None, cmax=None, 
                            scaler=None, heatmap=False):
    # Inverse transform the range to the actual scale
    if scaler is not None:
        x_range, y_range, t_range = inverse_transform(x_range, y_range, t_range, scaler)
    
    if cmin is None:
        cmin = 0
    if cmax is "outlier":
        cmax = np.max([np.max(lamb_st) for lamb_st in lambs])
    if cmax is None:
        cmax = np.max(lambs)
    frames = []

    for i, lamb_st in enumerate(lambs):
        if heatmap:
            frames.append(go.Frame(data=[go.Heatmap(z=lamb_st, x=x_range, y=y_range, zmin=cmin, 
                                                    zmax=cmax)], name="{:.2f}".format(t_range[i])))
        else:
            frames.append(go.Frame(data=[go.Surface(z=lamb_st, x=x_range, y=y_range, cmin=cmin, 
                                                    cmax=cmax)], name="{:.2f}".format(t_range[i])))
    
    fig = go.Figure(frames=frames)
    # Add data to be displayed before animation starts
    if heatmap:
        fig.add_trace(go.Heatmap(z=lambs[0], x=x_range, y=y_range, zmin=cmin, zmax=cmax))
    else:
        fig.add_trace(go.Surface(z=lambs[0], x=x_range, y=y_range, cmin=cmin, cmax=cmax))

    # Slider
    sliders = [
                  {
                       "pad": {"b": 10, "t": 60},
                       "len": 0.9,
                       "x": 0.1,
                       "y": 0,
                       "steps": [
                           {
                               "args": [[f.name], frame_args(0)],
                               "label": f.name,
                               "method": "animate",
                           }
                           for f in fig.frames
                       ],
                   }
               ]
    
    # Layout
    fig.update_layout(
        title='Spatio-temporal Conditional Intensity',
        width=600,
        height=600,
        scene=dict(
                   zaxis=dict(range=[cmin, cmax], autorange=False),
                   aspectratio=dict(x=1, y=1, z=1),
                  ),
        updatemenus = [
            {
                "buttons": [
                    {
                        "args": [None, frame_args(1)],
                        "label": "&#9654;", # play symbol
                        "method": "animate",
                    },
                    {
                        "args": [[None], frame_args(0)],
                        "label": "&#9724;", # pause symbol
                        "method": "animate",
                    },
                ],
                "direction": "left",
                "pad": {"r": 10, "t": 70},
                "type": "buttons",
                "x": 0.1,
                "y": 0,
            }
        ],
        sliders=sliders
    )
    fig.show()
    
    

class TrajectoryPlotter:
    
    def __init__(self):
        self.data = []
        self.layout = go.Layout(
            width  = 1200,
            height = 600,
            scene  = dict(
                camera=dict(
                    up  = dict(x=1, y=0., z=0),
                    eye = dict(x=0., y=2.5, z=0.)
                ),
                xaxis = dict(title="latitude"),
                yaxis = dict(title="longitude"),
                zaxis = dict(title="time"), 
                aspectmode  = "manual",
                aspectratio = dict(x=1, y=1, z=3)
            ),
            showlegend = True,
        )
        
    
    """
    outputs: [batch, lookahead, 3] or [batch, 3]
    targets: [batch, lookahead, 3] or [batch, 3]
    """
    def compare(self, outputs, targets):
        if len(targets.shape) == 2:
            outputs = np.expand_dims(outputs, 1)
            targets = np.expand_dims(targets, 1)
        
        target_t = np.append(0, np.cumsum(targets[:, 0, 2]))
        self.add_trace(targets[:, 0, 0], targets[:, 0, 1], target_t, "actual")
        
        n = outputs.shape[0]
        lookahead = outputs.shape[1]
        for i in range(lookahead):
            output_t = np.append(0, np.append(0, np.cumsum(targets[:n-i-1, 0, 2])) + outputs[:n-i, i, 2])
            self.add_trace(outputs[:n-i, i, 0], outputs[:n-i, i, 1], output_t, f"Predicted {i} step")
        
    
    """
    x, y, z: [batch]
    """
    def add_trace(self, x, y, z, name=None, color=None):
        self.data.append(go.Scatter3d(
                             x = x,
                             y = y,
                             z = z,
                             name = name,
                             mode = 'lines+markers',
                             marker = dict(
                                 size   = 4,
                                 symbol = 'circle',
                                 color  = color,
                             ),
                             line = dict(
                                 width  = 3,
                                 color  = color,
                             ),
                             opacity = .6
                        ))
        
        
    def show(self):
        fig = go.Figure(data=self.data, layout=self.layout)
        fig.show()


def animate_lambs_on_map_smopy(lambs, x_range, y_range, t_range, fps, scaler=None, cmin=None, cmax=None, 
                               history=None, decay=0.3, base_size=300, cmap='magma', fn='result.mp4'):
    """
    Creates an animation of lambs overlaid on a map using smopy.
    """
    # Inverse transform the range to the actual scale
    if scaler is not None:
        x_range, y_range, t_range = inverse_transform(x_range, y_range, t_range, scaler)
    if cmin is None:
        cmin = 0
    if cmax == "outlier":
        cmax = np.max([np.max(lamb) for lamb in lambs])
    if cmax is None:
        cmax = np.max(lambs)
    print(f'Inferred cmax: {cmax}')

    # Get bounds for the map
    lon_min, lon_max = x_range.min(), x_range.max()  # Longitude range
    lat_min, lat_max = y_range.min(), y_range.max()  # Latitude range

    # Initialize smopy map
    map_obj = smopy.Map((lat_min, lon_min, lat_max, lon_max), z=12)
    
    # Convert grid points to map coordinates
    def to_map_coords(lon, lat):
        return map_obj.to_pixels(lat, lon)

    # Get pixel coordinates of the heatmap extent
    px_x_min, px_y_min = to_map_coords(lon_min, lat_min)
    px_x_max, px_y_max = to_map_coords(lon_max, lat_max)

    # Ensure the extent is ordered correctly
    if px_y_min > px_y_max:
        px_y_min, px_y_max = px_y_max, px_y_min  # Swap to ensure correct ordering

    # Transform historical points if provided
    if history is not None:
        his_s, his_t = history
        his_px = np.array([to_map_coords(lon, lat) for lon, lat in his_s])
    else:
        his_px = None

    # Create the figure
    fig, ax = plt.subplots(figsize=(8, 8), dpi=150)
    ax.set_xlim(0, map_obj.img.width)
    ax.set_ylim(map_obj.img.height, 0)
    ax.set_title("Spatio-temporal Conditional Intensity", fontsize=12)

    # Display the basemap
    ax.imshow(map_obj.img, extent=[0, map_obj.img.width, map_obj.img.height, 0], origin='upper', zorder=0)

    # Initialize heatmap
    heatmap = ax.imshow(lambs[0], extent=[px_x_min, px_x_max, px_y_min, px_y_max],  # Match heatmap to basemap extent
                        origin='upper', cmap=cmap, alpha=0.7, zorder=1, vmin=cmin, vmax=cmax)

    # Add time annotation
    time_text = ax.text(10, 10, f"t = {t_range[0]:.2f}", fontsize=10, color='white', verticalalignment='top')

    # Add historical points (if provided)
    if his_px is not None:
        scatter = ax.scatter([], [], color='black', s=[], alpha=0.8, zorder=2)

    pbar = tqdm(total=len(t_range))

    def update(frame):
        # Update heatmap
        heatmap.set_data(lambs[frame])
        time_text.set_text(f"t = {t_range[frame]:.2f}")

        if his_px is not None:
            # Add historical points
            mask = np.logical_and(his_t <= t_range[frame], his_t >= t_range[0])
            locs = his_px[mask]
            times = his_t[mask]
            sizes = np.exp((times - t_range[frame]) * decay) * base_size
            scatter.set_offsets(locs)
            scatter.set_sizes(sizes)

        pbar.update()

    # Create animation
    ani = animation.FuncAnimation(fig, update, frames=len(t_range), interval=1000 / fps)

    # Save the animation
    ani.save(fn, writer='ffmpeg', fps=fps)
    plt.close(fig)
    pbar.close()

    return ani
