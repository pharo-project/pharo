private
prepareFilterInContext: aSpotterContext
	super prepareFilterInContext: aSpotterContext.
	
	separators ifNil: [  separators := self defaultSeparators  ]. " performance optimization "
	self query: (self query substrings: self separators) asSet asArray