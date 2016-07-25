*OpalTools
sourceInterval
	^self node 
		ifNil: [0 to: 0]
		ifNotNil: [:n | n sourceInterval]