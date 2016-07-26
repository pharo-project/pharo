accessing
selectVariables: aSelector
	^ variables select: [:v | v perform: aSelector] thenCollect: [:v | v name]