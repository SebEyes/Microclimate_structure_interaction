using Pkg
using CSV, DataFrames
using TidierData, Dates

## List files available in the directory
data_dir = "data/microclimate_data/copernicus_data/hourly_data"
files = readdir(data_dir, join = true)

sites_code = basename.(files)
sites_code = replace.(sites_code, r"\.csv" => "")

all_data = DataFrame()

for idx_file in eachindex(files)
    cop_data = CSV.read(files[idx_file], skipto = 44, DataFrame)
    cop_data = cop_data[:, [1,7]]
    rename!(cop_data, [:date, :GHI])
    cop_data = @separate(cop_data, date, (start, stop), "/")
    select!(cop_data, Not(:stop))
    cop_data.start = DateTime.(cop_data.start)
    rename!(cop_data, :start => :time_stamp)
    cop_data[!, :site_code] .= sites_code[idx_file]
    append!(all_data, cop_data)
end

select!(all_data, :site_code, :time_stamp, :GHI)

CSV.write("data/microclimate_data/copernicus_data/hourly_data/compiled_copernicus_GHI_data.csv", all_data)
