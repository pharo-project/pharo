lookup elements
elementNamed: elementName in: aCollection
	| aSymbol |
	
	aSymbol:= elementName asSymbol.
	^aCollection isDictionary
		ifTrue: [ aCollection at: aSymbol ifAbsent: [ nil ] ]
		ifFalse:[ aCollection detect:[ :each| each fullName = aSymbol ] ifNone:[ nil ] ]