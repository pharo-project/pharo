private
prepareFilterInContext: aSpotterContext
	super prepareFilterInContext: aSpotterContext.
	
	caseSensitive ifNil: [ caseSensitive := self defaultCaseSensitive ]. " performance optimization "
	self query: (caseSensitive " performance optimization "
		ifTrue: [ self context textTrimmed ]
		ifFalse: [ self context textLowercase ])