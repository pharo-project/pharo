Extension { #name : 'Morph' }

{ #category : '*System-Model' }
Morph >> myDependents [
	"Improved performance dependents."

	^ (self valueOfProperty: #myDependents) ifNil: [ #() ]
]

{ #category : '*System-Model' }
Morph >> myDependents: aCollectionOrNil [
	"Improved performance dependents."

	aCollectionOrNil
		ifNil: [self removeProperty: #myDependents]
		ifNotNil: [self setProperty: #myDependents toValue: aCollectionOrNil]
]
