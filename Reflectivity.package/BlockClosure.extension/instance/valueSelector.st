*Reflectivity
valueSelector
	self numArgs = 0 ifTrue: [^#value].
	^(String streamContents: [:stream | 
		stream nextPutAll: 'value:'.
		(self numArgs - 1) timesRepeat: [stream nextPutAll: 'value:']]) asSymbol.