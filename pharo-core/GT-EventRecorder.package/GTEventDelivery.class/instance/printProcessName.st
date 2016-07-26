printing
printProcessName
	^ String streamContents: [ :aStream |
		self printProcessNameOn: aStream ]