I am responsible for maintaining what (image) version of Pharo is running.
Usually my format is:

     Pharo-<major>.<minor>+<suffix>.build.<buildnumber>.sha.<commitHash>

following semver.org (but wothout a patch number). The metadata with suffix, build number
and commit hash are option.

Some queries are
	SystemVersion current major
	SystemVersion current minor
	SystemVersion current suffix
	SystemVersion current highestUpdate

For compatibility we also have

        SystemVersion current version
