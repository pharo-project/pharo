Extension { #name : 'ClassRepackaged' }

{ #category : '*Deprecated12' }
ClassRepackaged >> classRecategorized [

	self deprecated: 'Use #classRepackaged instead.' transformWith: '`@rcv classRecategorized' -> '`@rcv classRepackaged'.
	^ self classRepackaged
]

{ #category : '*Deprecated12' }
ClassRepackaged >> newCategory [

	self deprecated: 'Manipulated the tag and not the category.'.
	^ self newTag categoryName
]

{ #category : '*Deprecated12' }
ClassRepackaged >> oldCategory [

	self deprecated: 'Manipulated the tag and not the category.'.
	^ self oldTag categoryName
]
