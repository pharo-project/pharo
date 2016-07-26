lookup elements
elementNamed: elementName
	| aSymbol found |
	
	aSymbol:= elementName asSymbol.
	elements do:[ :collection| 		
		(found:= self elementNamed: aSymbol in: collection) notNil ifTrue:[ ^found ] ].
	^nil