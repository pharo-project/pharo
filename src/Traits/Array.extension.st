Extension { #name : 'Array' }

{ #category : '*Traits' }
Array >> asTraitComposition [

	^ self
		ifEmpty: [ TaEmptyComposition new ]
		ifNotEmpty: [ TaSequence withAll: (self collect: [ :each | each asTraitComposition ]) ]
]
