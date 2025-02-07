
[![img](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)

lfs_industry_profiles
============================

### Usage

* THIS PROJECT SHOULD LIVE ON YOUR HARD DRIVE, NOT THE LAN.

* Get an RTRA account. (application form in directory `SAS`) 
* upload the 4 .SAS files in directory `SAS` to https://www75.statcan.gc.ca/eft-tef/en/operations (To StatCan).
* grab a coffee...
* download the 4 resulting csv files (From StatCan) and place in directory "data/current".
* Click the `Git` button and `pull` to make sure the script is the most recent version.
* source file 00_source_me.R.

Note that Jan 2026 need to create new scripts to download most recent data.


#### Purpose

This script creates a mapping file from naics to three levels of aggregation, based on the "Beyond 20/20" custom tab from BC Stats (modified to breakdown health further).  The 4 digit RTRA employment data is then merged with the mapping file, and then the RTRA data is aggregated to the three levels of aggregation.  The aggregated data is written to an excel file with a sheet for each of the high level aggregates. An html dashboard is also created. Output found in directory out/current. 

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/lfs_industry_profiles/issues/).

### How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

### License

```
Copyright 2022 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
```
---
*This project was created using the [bcgovr](https://github.com/bcgov/bcgovr) package.* 
