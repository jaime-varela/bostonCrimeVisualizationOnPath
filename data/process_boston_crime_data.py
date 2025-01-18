# Copyright (c) Facebook, Inc. and its affiliates.

from tqdm import tqdm
import pandas as pd
import numpy as np
import reverse_geocoder as rg
from datetime import datetime


def main():
    np.random.seed(0)
    # These numbers need better motivation
    selection_probability = (1/3)
    num_samplings_to_repeat = 3

    date_parser = lambda x: pd.to_datetime(x, format="%Y-%m-%d %H:%M:%S", errors="coerce")

    # only analyzing the violent crimes
    df = pd.read_csv('violentCrimes.csv', encoding='latin-1',
                   parse_dates=['OCCURRED_ON_DATE'], date_parser=date_parser,dtype={'INCIDENT_NUMBER': 'str'})

    rename = {'OCCURRED_ON_DATE':'date'}

    df.rename(index=str, columns=rename, inplace=True)

    # Select time line of start and end date to analyze.
    # Get the start and end dates
    start_date = df['date'].min()
    end_date = df['date'].max()

    # Create numeric time column.
    df["day"] = df["date"].apply(lambda x: float((x - start_date).days))
    df["delta_hours"] = df["date"].apply(lambda x: float((x - start_date).total_seconds())/ 3600)

    # Cases in New Jersey.
    df = df[["day", "Long", "Lat","delta_hours" ]]

    # Break into 7 day intervals using a sliding window of 3 days.
    sequences = {}
    interval_length = 7
    for start in range(0, int(df["day"].max()) - interval_length + 1, 3):

        total = len(range(0, int(df["day"].max()) - interval_length + 1, 3))
        print(f'start = {start}, total = {total}')
        print(f'percent done {start/total}')

        date = start_date + pd.Timedelta(days=start)
        seq_name = f"{date.year}{date.month:02d}" + f"{date.day:02d}"

        df_range = df[df["day"] >= start]
        df_range = df_range[df_range["day"] < start + interval_length]
        # FIXME this time should be cyclicy and use the hour representation not the day representation
        df_range["day"] = df_range["day"] - start
        delta_hours_at_start = (date - start_date).total_seconds() / 3600
        df_range["delta_hours"] = df_range["delta_hours"] - delta_hours_at_start

        seq = df_range.to_numpy()[:, :4].astype(np.float64)
        counties = df_range.to_numpy()[:, -1]

        t, x = seq[:, 3:4], seq[:, 1:3]

        print(seq_name, seq.shape[0])

        for i in tqdm(range(num_samplings_to_repeat)):
            # subsample_idx = np.sort(np.random.choice(seq.shape[0], seq.shape[0] // 200, replace=False))
            subsample_idx = np.random.rand(seq.shape[0]) < selection_probability

            while np.sum(subsample_idx) == 0:
                subsample_idx = np.random.rand(seq.shape[0]) < selection_probability

            # Uniformly distribute the daily case count.
            _t = add_temporal_noise(t[subsample_idx])

            # Assume each degree of longitude/latitude is ~110km.
            _x = add_noise_to_coordinates(x[subsample_idx])

            sort_idx = np.argsort(_t.reshape(-1))
            sequences[seq_name + f"_{i:03d}"] = np.concatenate([_t, _x], axis=1)[sort_idx]

    np.savez("boston_violent_crimes.npz", **sequences)


def add_noise_to_coordinates(coords, max_distance_m=5):
    """
    Add random noise within a specified distance to an array of longitude, latitude coordinates.
    
    Parameters:
        coords (np.ndarray): An array of shape (N, 2) where each row is [longitude, latitude].
        max_distance_m (float): The maximum distance for the noise in meters.
        
    Returns:
        np.ndarray: The array with noisy coordinates.
    """
    # Earth's parameters
    earth_radius_m = 6371000  # Earth's radius in meters
    km_per_deg_lat = 111.32  # Approximate kilometers per degree latitude

    # Convert max distance from meters to kilometers
    max_distance_km = max_distance_m / 1000
    
    # Degrees per distance
    max_distance_deg_lat = max_distance_km / km_per_deg_lat
    max_distance_deg_lon = max_distance_km / (km_per_deg_lat * np.cos(np.deg2rad(coords[:, 1])))

    # Generate random noise in degrees
    noise_lat = np.random.uniform(-max_distance_deg_lat, max_distance_deg_lat, size=len(coords))
    noise_lon = np.random.uniform(-max_distance_deg_lon, max_distance_deg_lon, size=len(coords))
    
    # Add the noise to the coordinates
    noisy_coords = coords + np.column_stack((noise_lon, noise_lat))
    return noisy_coords


def add_temporal_noise(hours, noise_minute_bound = 5):
    # minutes in hours
    five_minutes_in_hours = noise_minute_bound / 60

    # Generate random noise between 0 minutes and +5 minutes in hours
    noise = np.random.uniform(0, five_minutes_in_hours, size=hours.shape)

    # Add noise to the original array
    time_deltas_noisy = hours + noise

    return time_deltas_noisy

if __name__ == "__main__":
    main()
