# API options

## Typical flow:

- download data pack - select type and period(s)
- inspect data pack: components and their dimensions
- load selected component (dataset) of data pack
- process data in part of data pack
	- includes: turning around negative values; fixing data types, padding etc.; 
	- marking sums?
- enrich data pack
	- download and process ciselnik on demand (process = turn into df + rename cols for joining + formatting cols incl. data types, padding etc.)
	- download and link generic ciselnik (org codes, currency codes)
	- link ciselnik in a time-appropriate way (filter and link)
- merge periods
- filter for org

## Alternative - more expert:

return <number> for each {org}, {year}, {indicator/code/line item} in a given {report} and perhaps {budget state}.

e.g.

- extract_accounts("Praha 1", 2010:2015, "Kapitálové výdaje celkem", "final"), and 
- extract_budget("Praha 1", 2010:2015, "Kapitálové výdaje celkem") or
- extract_series("Praha", 2010:2015, "accounts", "")

# Technical:

use vroom?
specify column types
specify column names?
