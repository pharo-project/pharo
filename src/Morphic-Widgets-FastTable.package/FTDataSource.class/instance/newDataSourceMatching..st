accessing
newDataSourceMatching: aRegex
	"I am here to help the FTFilterFunction. I take a Regex and I need to return a new dataSource with elements that match the regex. Each dataSource is responsible of his filtering in order to get a generic mechanism."
	
	"the default action is to just do nothing, return self"
	
	^self