
# Where to put green infrastructure

Humphrey students presented on the fact that there are lots of variables
to consider when selecting geographic areas where green roofs (or other
green infrastructure). The students looked at a suite of variables, and
established clear cut-off zones for determining which tracts where
greenroofs could have a disproportionate benefit. For instance, tracts
where the average summer temperature on a hot day was &gt;= 95 degrees,
and tracts must be an “area of concentrated poverty.”

<br> Leveraging the continuous nature of these variable rather than
binning them into “suitable for green roofs” versus “not suitable” could
be a way to give a bit more nuance into the conversation. Here, explore
the full range of summer temperatures, or percent of people within a
tract whose income is in the bottom 20% of the region. By synthesize
these disparate variables into a single, understandable value, we can
identify the tracts where greenroofs could have the largest
disproportionate benefit. And also compare the differences in “how much
better” is one tract versus another.

<br> Here we’ve centered and re-scaled all the variables, so they all
have the same units. Please scroll down to start using this tool. We
suggest you use a desktop web browser for the best experience.

<br> This is totally a quick and dirty test to see if this works. All
variables used here are pulled from the Equity Considerations Dataset
and informed by the student presentation on Tuesday. If this seems like
a useful approach (synthesizing the different variables by using
standardized and scaled values), let’s think more critically about what
the data inputs should be!

<br>

## Things to do - Ellen

heirarchical partitioning to show that urban heat islands are driven by
commercial/industrial land uses, and that green infrastructure can
mitigate those consquences

tool touches several aspects: - what is green infrastructure - what can
green infrastrcuture do - what can i do about it? implementation; cost
benefits; etc

who this is for: - MN dept of health; air quality - DNR; habitat - MPCA
- cities, counties - watershed districts - water treatment plants -
maintence folks - paving, salting roads - developmers - materials,
structural, archetictual folks - ecologists/biologists; carbon
sequestration/habitat - advocates; equity

benefits of green infrastructure - carbon sequestration - food systems
(toronto) - urban heat islands - localized flooding; infiltration,
groundwater recharge - air quality - biodiversity

solar KWH; thousands 1,000,000,000 11,000,000 1,500,000 200,000 90,000

greenroof stormwater heat carbon airquality

US Energy Information Administration (EIA) says 77.0 million btu per
capita in MN
<https://www.eia.gov/state/seds/data.php?incfile=/state/seds/sep_sum/html/rank_use_capita.html&sid=US>
<https://www.eia.gov/state/seds/seds-data-complete.php?sid=MN#CompleteDataFile>
; click on 1960-2018 csv file under “full reports and data files”. Then
filter to get: TERPB Total energy consumption per capita in the
residential sector Million Btu

``` r
# Approximate definition: A British thermal unit is defined as the amount of heat required to raise the temperature of one pound of water by one degree Fahrenheit. A kitchen match has about 1 btu of heat energy
# it looks like the eia has converted it all to btus
# 1 kilowatt hour of electricity = 3,412 Btu
# 1 cubic feet of natural gas = 1037 Btu

minneap_potential_kwh <- 1e12
smbldg_potential_kwh <- 4e6
av_consumption_btu <- 77e6 #Total energy consumption per capita in the residential sector
av_consumption_kwh <- av_consumption_btu / 3412

percap_consumption_btu <- 341.4e6 #Total energy consumption per capita
percap_consumption_kwh <- percap_consumption_btu / 3412

#houses by minneapolis
minneap_potential_kwh / (av_consumption_kwh * 4) / 30 #30 years being an average lifespan
```

    ## [1] 369264.1

``` r
smbldg_potential_kwh / (av_consumption_kwh * 4)
```

    ## [1] 44.31169

``` r
# people
minneap_potential_kwh / percap_consumption_kwh 
```

    ## [1] 9994142

``` r
1e12 / (77e6 / 3412) / 4 / 30
```

    ## [1] 369264.1

# Solar potential:

The US Energy Information Administration (EIA) data gives the total
energy consumption per capita in the residential sector within Minnesota
as 77.0 million Btus for the year 2018. This number accounts for the
different energy/fuel sources (e.g. electricity, natural gas) used
within the residential sector. Given that solar potential in the surface
with purpose tool is presented as kilowatt hours of electricity, we will
use the 1kWh = 3412 Btu conversion factor.

I would recommend further distilling solar potential to a values such as
"offsetting the residential energy use of X families (of 4?) for 30
years (the average life of a solar installation). Looking at the
lifespan of the solar array might only be useful when looking at
city-wide aggregate numbers.

So for instance, the entire city of Minneapolis has a solar potential of
about 2 billion kWh (or maybe 2e12 … it’s not totally clear that the
tooltip info (kWh in thousands) lines up with the overall y-axis units
(in kWh…not thousands)). Which means that if every roof was converted to
solar in Minneapolis AND had realized it’s full solar potential
(probably a lofty goal!), this would mean that \~740,000 families of 4
would have their residential energy use offset for 30 years!! (2e12 kWh
/ (77e6 Btu / 3412 kWh \* 4 people \* 30 years)).

# Green roof:

An olympic swimming pool (50 m long x 25 m wide x 2 m deep) holds 2500
cubic m of water, or 88,286.7 cubic feet of water. This seems like an
appropriate conversion factor at the city-wide level.

However, it seems like greenroofs should actually be linked to lowering
land temperature. So a different measurement might actually be in order
(using greenroof area, rather than cubic feet of water retained).
<https://www.epa.gov/heatislands/using-green-roofs-reduce-heat-islands>

<hr>
