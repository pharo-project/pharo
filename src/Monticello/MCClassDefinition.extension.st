Extension { #name : 'MCClassDefinition' }

{ #category : '*Monticello' }
MCClassDefinition >> accept: aVisitor [
	aVisitor visitClassDefinition: self.
	(self hasClassInstanceVariables or: [self hasClassTraitComposition])
		ifTrue: [aVisitor visitMetaclassDefinition: self].

]

{ #category : '*Monticello' }
MCClassDefinition >> storeDataOn: aDataStream [
	| instVarSize |
	instVarSize := (self hasTraitComposition or: [ self hasClassTraitComposition ])
		ifTrue: [ self class instSize ]
		ifFalse: [ self class instSize - 2 ].
	aDataStream
		beginInstance: self class
		size: instVarSize.
	1 to: instVarSize do: [ :index |
		aDataStream nextPut: (self instVarAt: index) ].
]
