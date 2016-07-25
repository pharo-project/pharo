instance variables
instVarIndexFor: instVarName ifAbsent: aBlock
	"Answer the index of the named instance variable."

	| index |
	index := self instVarNames indexOf: instVarName ifAbsent: [0].
	index = 0 ifTrue: 
		[^self superclass == nil 
			ifTrue: [aBlock value]
			ifFalse: [self superclass instVarIndexFor: instVarName ifAbsent: aBlock]].
	^self superclass == nil 	
		ifTrue: [index]
		ifFalse: [index + self superclass instSize]