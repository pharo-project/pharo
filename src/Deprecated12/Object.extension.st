Extension { #name : #Object }

{ #category : #'*Deprecated12' }
Object >> asOrderedCollection [

	self
		deprecated:
		'The usage of this method is not recommended. We want to have a smaller Object api. We will remove this method in the next Pharo version.'
		transformWith: '`@receiver asOrderedCollection' -> 'OrderedCollection with: `@receiver'.
	^ OrderedCollection with: self
]
