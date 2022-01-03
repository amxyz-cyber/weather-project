# Data Visualization - Ford GoBike (Bike Shares)


## Dataset
The datasets, [results-milan.csv](https://github.com/amxyz-cyber/weather-project/blob/master/data/results-milan.csv) 
and [results-globa.csv](https://github.com/amxyz-cyber/weather-project/blob/master/data/results-global.csv) 
were originally provided by Udacity. The datasets include 266 global 
temperatures from 1750 until 2015 as well as 271 temperatures of Milan 
collected between 1743 and 2013. The main features are of course the 
year and the average temperature for a whole year. To plot the moving 
averages I created the haskell programm `weatherComparator` which reads 
in both csv files. The moving averages of both regions are calculated 
based on an interval of 10 years. If a year had no temperature then this
row was dropped. The calculated moving averages were written to the csv
file [moving-average-weather-data.csv](https://github.com/amxyz-cyber/weather-project/blob/master/data/moving-average-weather-data.csv).
Last but not least the haskell program [plotted](https://github.com/amxyz-cyber/weather-project/blob/master/submission/chart3.png) 
the csv file which showed the moving average global temperatures vs. 
the moving average temperatures of Milan.


## Summary of Findings
1. In general, the moving average temperatures of Milan are lower 
compared to the global moving averages.

2. Both categories, i.e. 'global' and 'Milan', show the same trend that the 
average temperature has been rising, especially since around 1945.

## Review
* [Udacity Review ](https://github.com/amxyz-cyber/weather-project/blob/master/submission/review-project01.pdf)  

## WeatherComparator
> The `WeatherComparator` is a program written in Haskell. It creates 
the moving averages based on two csv files that need to have the 
headings `year` and `avg_temp` (see examples in the [data folder](https://github.com/amxyz-cyber/weather-project/tree/master/data)).

> Usage: weatherComparator [-h] [-c csv-file1] [-c csv-file2] [-d link] [-t] [-r region1] [-r region2]
  -v          --verbose         chatty output on stderr
  -V, -?      --version         show version number
  -q          --quit            end program
  -c FILES    --csv=FILES       csv file for temperature comparison
  -d LINK     --chart=LINK      create a diagram
  -t          --test            create a diagram based on test set
  -r REGIONS  --region=REGIONS  the name of a region
  -h          --help            Show this help message

> First of all, you can run this program by generating the csv file
'moving-average-weather-data.csv' as well as the htmlfile 
'moving-averages.html' containing the diagram by using the testset:
`weatherComparator -t -v`

> Another option is to generate the csv file 
'moving-average-weather-data.csv' based on your data, for example:
`weatherComparator -v -r "San Francisco" -r Milan -c results-milan.csv -c results-sanfrancisco.csv`

> The third option is to generate the html file based on your csv file 
containing the moving averages. In order to generate the html file your
csv file needs to contain the headings `year`, `10-year_MA` as well as
 `data`. Example:
`weatherComparator -v -r "San Francisco" -r Milan -c moving-average-weather-data.csv -d https://raw.githubusercontent.com/amxyz-cyber/data/main/moving-average-weather-data2.csv`

> Hint: You can leave out the regions but then the default values 'region1'
and 'region2' will be used instead.

### Running the program:
> This repositary currently contains binaries for the Raspberry Pi 
'aarch64' architecture as well as ia64 (https://github.com/amxyz-cyber/weather-project/tree/master/binaries).
You will also need the haskell-platform which basically includes the
ghc-compiler and its dependencies.

> `weatherComparator` or `./weatherComparator`

### Install the project
1. If you want to compile the programm by yourself then please download
[stack](https://docs.haskellstack.org/en/v1.9.3/maintainers/releases/).
> After installing stack and having downloaded this project go to the
directory containing the project `weather-project` and enter the 
following commands:

2. `stack build`

3. If Stack complains about that it can't find the lts build or any link
provided in packages.yaml is invalid then you need to upgrade stack first:
`stack upgrade`

4. After compiling the project you can run the program by entering the
following command:
`stack exec weather-project-exe`

> Note, that the stack command should be available globally. If you can
use stack only locally, i.e. `./stack` then you need to export the path
to the folder containing its binary, for example:
> $ `export PATH="${PATH}:/home/username/.local/bin"`

> The compiled binary will be placed in a subfolder of this project 
depending on the architecture. For example, for 'aarch64' you'll find the
binary in: '.stack-work/dist/aarch64-linux/Cabal-3.2.1.0/build/weather-project-exe/weather-project-exe'

5. To finally install the binary, you'll use the following command:
> `stack install` 
> This command copies the binary to your local bin folder, for example:
'/home/username/.local/bin'. You can rename the binary by simply using
the move command 'mv' to `weatherComparator`. This will work, too.





  
