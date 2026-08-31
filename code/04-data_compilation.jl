# ---------------------------------------------------
# 1. LOAD PACKAGES
# ---------------------------------------------------

using Pkg
using CSV, DataFrames
using StatsBase
using Dates


# ---------------------------------------------------
# 2. LOAD AND PREP DATA
# ---------------------------------------------------
MC = CSV.read("data/microclimate_data/data_sensors/daily_stats_corrected/daily_stats_V3_corrected.csv", missingstrings = "NA", DataFrame)
radiation = CSV.read("data/microclimate_data/copernicus_data/hourly_data/compiled_copernicus_GHI_hourly_data.csv", DataFrame)
structure = CSV.read("data/vegetation_data/LiDAR/structure_data.csv", DataFrame)

filter!(row -> row.site_code != "TER-NFSB-TE49", MC)  # Remove Lagoinha (no struture data)
filter!(row -> row.site_code != "TER-NFSB-TE49", radiation)    # Remove Lagoinha (no struture data)


# Merge structure info into temporal data by plot_id

# Average structural variables per plot (locationID)
structure_avg = combine(groupby(structure, :locationID), names(structure, Not(:locationID)) .=> mean, renamecols = false)
rename!(structure_avg, Dict(:locationID => :site_code))
col = [
    :site_code,
    :canopy_openess,
    :foliage_height_diversity,
    :UCI,
    :ENL0D,
    :ENL1D,
    :SSCI,
    :Aspect_30,
    :Elevation,
    :Slope_30,
]
select!(structure_avg, col)


## Compute daily range
MC.DTR .= (MC.temperature_max - MC.temperature_min);
MC.DVPDR .= MC.vpd_kpa_max - MC.vpd_kpa_min;



radiation.date .= Date.(radiation.time_stamp)

radiation = combine(
    groupby(
        radiation,
        [:site_code, :date]
    ),
    :GHI => maximum => :DGHIR,
    :GHI => x -> count(!=(0), x),
    :GHI
)
rename!(radiation, Dict(:GHI_function => :num_hours_recorded))


radiation.DGHIR .= @. radiation.DGHIR / radiation.num_hours_recorded;

# Melting MC data
select!(MC, Not(:num_hours_recorded))

MC_long = stack(
    MC,
    [
        :DTR,
        :DVPDR,
        :temperature_mean,
        :vpd_kPA_mean
    ],
    [
        :site_code,
        :strata,
        :date
    ],

)

MC_long.variable .= @. MC_long.strata *"_"* MC_long.variable
    
MC = unstack(MC_long , [:site_code, :date], :variable, :value)

# Merging all data 
MC_data = leftjoin(MC, select(radiation, Not(:num_hours_recorded)), on = [:site_code, :date])
describe(MC_data)


dataset_modeling = leftjoin(MC_data, structure_avg, on = :site_code)

describe(dataset_modeling)

CSV.write(
    "data/microclimate_data/data_modelling.csv"
    , dataset_modeling
)
