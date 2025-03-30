import pandas as pd
import uuid
from io import StringIO



def open_qualtrics_csv_as_dfs(filename, id_field='id'):
    # Create a DataFrame from the data portion, using the first header line (column names)
    df_results = pd.read_csv(filename)
    df_results_headers = df_results[0:2]
    df_results = df_results[2:]
    # Ensure id column is treated as string
    df_results[id_field] = df_results[id_field].astype(str)

    return df_results_headers, df_results

def open_csv_as_df(filename, id_field='id_code'):
    df = pd.read_csv(filename, skiprows=1)
    df[id_field] = df[id_field].astype(str)

    return df

def df_create_id_mapping(df):
    # Create a mapping: generate a new random UUID for each unique original id
    return {orig_id: str(uuid.uuid4()) for orig_id in df['id'].unique()}

def anonymize_qualtrics_results_csv(df_headers, df, id_mapping, output_file):
    # Replace the id columns using the mapping
    df['id'] = df['id'].map(id_mapping)

    # Write the anonymized df_results to a new CSV file,
    # writing the original header lines first, then the processed data without rewriting the header.
    with open(output_file, 'w', encoding='utf-8') as f:
        df_headers.to_csv(f, index=False)
        df.to_csv(f, index=False, header=False)


def anonymize_results_csv(df, id_mapping, output_file):
    # Load the prescreen CSV (and convert the id_code column to string)

    # Replace the id columns using the mapping
    df['id_code'] = df['id_code'].map(id_mapping)

    # Write the anonymized prescreen data to a new CSV file
    df.to_csv(output_file, index=False)


if __name__ == '__main__':
    # s1_results_in = 'data/Thesis+-+Trusting+the+Machine_January+18,+2025_16.09_coded.csv'
    # s1_results_out = 'data/anonymized/s1/anonymized_data.csv'
    # s1_data_headers, s1_data = open_qualtrics_csv_as_dfs(s1_results_in)
    # s1_id_mapping = df_create_id_mapping(s1_data)
    # anonymize_qualtrics_results_csv(s1_data_headers, s1_data, s1_id_mapping, s1_results_out)

    s2_results_in = 'data/raw/s2/Thesis+-+Trusting+the+Machine+-+Interactive_coded.csv'
    s2_prescreen_in = 'data/raw/s2/prescreen_data.csv'

    s2_results_out = 'data/anonymized/s2/anonymized_data.csv'
    s2_prescreen_out = 'data/anonymized/s2/anonymized_prescreen_data.csv'

    s2_data_headers, s2_data = open_qualtrics_csv_as_dfs(s2_results_in)
    s2_id_mapping = df_create_id_mapping(s2_data)
    anonymize_qualtrics_results_csv(s2_data_headers, s2_data, s2_id_mapping, s2_results_out)

    s2_prescreen_data = open_csv_as_df(s2_prescreen_in)
    anonymize_results_csv(s2_prescreen_data, s2_id_mapping, s2_prescreen_out)
