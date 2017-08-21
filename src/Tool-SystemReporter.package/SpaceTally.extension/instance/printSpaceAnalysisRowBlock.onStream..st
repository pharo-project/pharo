*Tool-SystemReporter
printSpaceAnalysisRowBlock: classNameLength onStream: stream
	^ [ :class :codeSpace :instanceCount :instanceSpace :percent | 
	stream
		nextPutAll: (class padded: #right to: classNameLength + 1 with: $ );
		nextPutAll: (codeSpace padded: #left to: 12 with: $ );
		nextPutAll: (instanceCount padded: #left to: 12 with: $ );
		nextPutAll: (instanceSpace padded: #left to: 14 with: $ );
		nextPutAll: (percent padded: #left to: 8 with: $ );
		cr ]