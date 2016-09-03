accessing
newDataSourceMatching: aFTFilter
	"I delegate the filter to a FTTreeFunctionStrategy."

	^ (self class searchStrategies at: self searchStrategy ifAbsent: [ self class searchStrategies at: #default ]) filterWith: aFTFilter pattern dataSource: self