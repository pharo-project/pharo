printing
shortPrintString
	self isEmpty
		ifTrue: [ ^ self printString ].
	^ String streamContents: [ :out | self printFrequenceOn: out ]