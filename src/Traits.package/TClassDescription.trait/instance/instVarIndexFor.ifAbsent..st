instance variables
instVarIndexFor: instVarName ifAbsent: aBlock
	"Answer the index of the named instance variable."

	| index |
	index := self classLayout instVarIndexFor: instVarName ifAbsent: 0.
	index = 0 ifTrue: 
		[^self superclass
			ifNil: [aBlock value]
			ifNotNil: [self superclass instVarIndexFor: instVarName ifAbsent: aBlock]].
	^self superclass
		ifNil: [index]
		ifNotNil: [index + self superclass instSize]