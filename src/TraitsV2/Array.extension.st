Extension { #name : 'Array' }

{ #category : '*TraitsV2' }
Array >> asTraitComposition [

	^ self
		ifEmpty: [ TaEmptyComposition new ]
		ifNotEmpty: [ TaSequence withAll: (self collect: [ :each | each asTraitComposition ]) ]
]
