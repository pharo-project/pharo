processing
newContextForQuery: aString
	^ GTSpotterContext new 
		step: self;
		stream: self stream;
		text: aString;
		search: aString;
		yourself