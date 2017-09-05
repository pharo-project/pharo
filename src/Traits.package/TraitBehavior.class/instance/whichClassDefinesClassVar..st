queries
whichClassDefinesClassVar: aString 
	Symbol hasInterned: aString ifTrue: [ :aSymbol |
		^self whichSuperclassSatisfies: 
			[:aClass | 
			aClass classVarNames anySatisfy: [:each | each = aSymbol]]].
	^#()