using Pkg

## Set up Julia env
Pkg.activate(".")
Pkg.add(["CSV", "DataFrames", "TidierData", "Dates", "Statistics", "Psychrometrics"])

using CSV
using DataFrames
using Dates
using Statistics
using Psychrometrics

data_path = "data/microclimate_data/data_sensors/field_data_collection"
output_path = "data/microclimate_data/data_sensors/"

# Get a list of all site directories
site_dirs = filter(isdir, readdir(data_path, join=true))

# Initialize an empty DataFrame to store all sites' data
all_sites_data = DataFrame()

# Iterate over each site directory
for site_dir in site_dirs
    # Get all .txt files in the site directory
    txt_files = filter(f -> endswith(f, ".txt"), readdir(site_dir, join=true))
    # Concatenate all data in txt_files and add a column with the corresponding basename of the file path
    site_data_list = DataFrame()
    for file in txt_files
        file_data = select(CSV.read(file, DataFrame), 2:4)
        rename!(file_data, [:time, :temperature, :humidity])
        file_data[!, :file_basename] .= basename(file)
        file_data[!, :site_code] .= basename(site_dir)  # Add site_code column
        file_data[!, :timestamp] .= String.(file_data[!, :time])  # Add timestamp column
        site_data_list = vcat(site_data_list, file_data)
    end
    all_sites_data = vcat(all_sites_data, site_data_list)
end

# Add a strata column based on the file_basename
all_sites_data[!, :strata] .= ifelse.(occursin.("-0-", all_sites_data[!, :file_basename]), "GRD",
    ifelse.(occursin.("-1-", all_sites_data[!, :file_basename]), "UND", "CAN"));

# Convert the timestamp column to DateTime format
all_sites_data[!, :timestamp] .= DateTime.(all_sites_data[!, :timestamp], dateformat"yyyy-mm-dd HH:MM:SS");

# Replace "EXO-T01" with "TER-EXO-T01" in the site_code column
all_sites_data[!, :site_code] .= replace.(all_sites_data[!, :site_code], r"^EXO-T01" => "TER-EXO-T01")
all_sites_data[!, :site_code] .= replace.(all_sites_data[!, :site_code], r"^EXO-T02" => "TER-EXO-T02")
all_sites_data[!, :site_code] .= replace.(all_sites_data[!, :site_code], r"^EXO-T03" => "TER-PRIBS-T28")
all_sites_data[!, :site_code] .= replace.(all_sites_data[!, :site_code], r"^EXO-T04" => "TER-EXO-T04")
all_sites_data[!, :site_code] .= replace.(all_sites_data[!, :site_code], r"^EXO-T05" => "TER-PRIBS-T09")
all_sites_data[!, :site_code] .= replace.(all_sites_data[!, :site_code], r"^EXO-T06" => "TER-PRIBS-T15")
all_sites_data[!, :site_code] .= replace.(all_sites_data[!, :site_code], r"^EXO-T07" => "TER-PRIBS-T27")
all_sites_data[!, :site_code] .= replace.(all_sites_data[!, :site_code], r"^EXO-T08" => "TER-PRIBS-T06")
all_sites_data[!, :site_code] .= replace.(all_sites_data[!, :site_code], r"^EXO-T09" => "TER-EXO-T09")
all_sites_data[!, :site_code] .= replace.(all_sites_data[!, :site_code], r"^EXO-T10" => "TER-EXO-T10")
all_sites_data[!, :site_code] .= replace.(all_sites_data[!, :site_code], "T01" => "TER-NFBF-T-01")
all_sites_data[!, :site_code] .= replace.(all_sites_data[!, :site_code], "T02" => "TER-NFBF-T-02")
all_sites_data[!, :site_code] .= replace.(all_sites_data[!, :site_code], "T07" => "TER-NFSB-T-07")
all_sites_data[!, :site_code] .= replace.(all_sites_data[!, :site_code], "T15" => "TER-NFTB-T-15")
all_sites_data[!, :site_code] .= replace.(all_sites_data[!, :site_code], "T164B" => "TER-NFSB-T164B")
all_sites_data[!, :site_code] .= replace.(all_sites_data[!, :site_code], "T18" => "TER-NFTB-T-18")
all_sites_data[!, :site_code] .= replace.(all_sites_data[!, :site_code], "T33" => "TER-NFPG-T-33")
all_sites_data[!, :site_code] .= replace.(all_sites_data[!, :site_code], "TE48" => "TER-NFSB-TE48")
all_sites_data[!, :site_code] .= replace.(all_sites_data[!, :site_code], "TE49" => "TER-NFSB-TE49")
all_sites_data[!, :site_code] .= replace.(all_sites_data[!, :site_code], "TP41" => "TER-NFBF-TP41")

all_sites_data[!, :site_code] .= replace.(all_sites_data[!, :site_code], "TER-EXO-TER-NFBF-T-01" => "TER-EXO-T01")
all_sites_data[!, :site_code] .= replace.(all_sites_data[!, :site_code], "TER-EXO-TER-NFBF-T-02" => "TER-EXO-T02")
all_sites_data[!, :site_code] .= replace.(all_sites_data[!, :site_code], "TER-EXO-TER-NFSB-T-07" => "TER-EXO-T07")

describe(all_sites_data)


metadata_collection = CSV.read("data/microclimate_data/data_sensors/data_logger_metadata.csv", DataFrame)
bad_data = filter(metadata_collection) do row
    occursin("Data logger on the ground", row[:remarks])
end
# Replace "EXO" with "TER-EXO" in the site_code column for matching rows
bad_data[!, :site_code] .= replace.(bad_data[!, :site_code], r"^EXO" => "TER-EXO")

# Parse start_time and end_time columns into DateTime format
bad_data[!, :start_time] .= DateTime.(bad_data[!, :start_time], dateformat"dd/mm/YYYY HH:MM");
bad_data[!, :end_time] .= DateTime.(bad_data[!, :end_time], dateformat"dd/mm/YYYY HH:MM");

bad_data[!, :strata] .= ifelse.(bad_data[!, :position] .== 0, "GRD",
    ifelse.(bad_data[!, :position] .== 1, "UND", "CAN"));


# Iterate over each row in bad_data to filter out the corresponding bad time ranges
for row in eachrow(bad_data)
    site_code = row[:site_code]
    strata = row[:strata]
    start_time = row[:start_time]
    end_time = row[:end_time]
    # Filter out rows from all_sites_data that match the site_code and fall within the time range
    all_sites_data = filter(row -> !(row[:site_code] == site_code && row[:strata] == strata && row[:timestamp] >= start_time && row[:timestamp] <= end_time), all_sites_data)
end


# # # Save the combined DataFrame to a single output file
# output_file = joinpath(output_path, "all_sites_combined_data.csv")
# CSV.write(output_file, all_sites_data)

# Add a new column `date` by extracting the day from the `timestamp` column
all_sites_data[!, :date] = Date.(all_sites_data[!, :timestamp])
all_sites_data[!, :hour] = hour.(all_sites_data[!, :timestamp])


# Filter day with 24H of sampling
temporal_info = select(all_sites_data, :site_code, :strata, :date, :hour)
temporal_info = combine(
    groupby(
        temporal_info,
        [:site_code, :strata, :date]
    ),
    :hour => length => :num_hours_recorded
)

temporal_info.valid_sampling = ifelse.(temporal_info.num_hours_recorded .== 24, true, false)

all_sites_data = innerjoin(
    all_sites_data,
    temporal_info,
    on = [:site_code, :strata, :date]
)

all_sites_data = filter(row -> row.valid_sampling == true, all_sites_data)



# Correct saturated value on sensor
all_sites_data.humidity[all_sites_data.humidity .> 100] .= 100

# compute vapor pressure deficit (VPD) in Pa

all_sites_data.vpd_PA .= satPress.(all_sites_data.temperature .+ 273.15) .- (all_sites_data.humidity ./ 100 .* satPress.(all_sites_data.temperature .+ 273.15))
# Convert VPD from Pa to kPA
all_sites_data.vpd_kPA .= all_sites_data.vpd_PA ./ 1000

daily_stats = combine(groupby(all_sites_data, [:site_code, :strata, :date, :num_hours_recorded]), 
    :temperature => mean => :temperature_mean,
    :temperature => std => :temperature_std,
    :temperature => maximum => :temperature_max,
    :temperature => minimum => :temperature_min,
    :vpd_kPA => mean => :vpd_kPA_mean,
    :vpd_kPA => std => :vpd_kPA_std,
    :vpd_kPA => maximum => :vpd_kpa_max,
    :vpd_kPA => minimum => :vpd_kpa_min,
)

daily_stats[!, :site_code] .= replace.(daily_stats[!, :site_code], "TER-PRIBS-TER-NFTB-T-15" => "TER-PRIBS-T15")

# Save the daily statistics to a CSV file
output_file_stats = joinpath(output_path, "daily_stats_V3.csv")
CSV.write(output_file_stats, daily_stats)