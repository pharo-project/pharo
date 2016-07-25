private
prepareFilterInContext: aSpotterContext
	super prepareFilterInContext: aSpotterContext.
	
	self regex: (caseSensitive " performance optimization "
		ifTrue: [ self query asRegex ] 
		ifFalse: [ self query asRegexIgnoringCase ])