converting
stringFromPath: aPath
	^ String streamContents: [ :out | 
		self printPath: aPath on: out ]