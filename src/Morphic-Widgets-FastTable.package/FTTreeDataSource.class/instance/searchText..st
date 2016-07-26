accessing
searchText: aString
	"I delegate the search to a FTTreeFunctionStrategy."

	^ (self class searchStrategies at: self searchStrategy ifAbsent: [ self class searchStrategies at: #default ]) searchWith: aString dataSource: self