accessing
newDataSourceMatching: aRegex
	"I delegate the filter to a FTTreeFunctionStrategy."

	^ (self class searchStrategies at: self searchStrategy ifAbsent: [ self class searchStrategies at: #default ]) filterWith: aRegex dataSource: self