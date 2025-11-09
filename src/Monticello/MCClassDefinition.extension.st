Extension { #name : 'MCClassDefinition' }

{ #category : '*Monticello' }
MCClassDefinition >> accept: aVisitor [
	aVisitor visitClassDefinition: self.
	(self hasClassInstanceVariables or: [self hasClassTraitComposition])
		ifTrue: [aVisitor visitMetaclassDefinition: self].

]
