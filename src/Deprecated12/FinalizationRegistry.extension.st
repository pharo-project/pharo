Extension { #name : 'FinalizationRegistry' }

{ #category : '*Deprecated12' }
FinalizationRegistry >> add: anObject executor: finalizer [

	self
		deprecated: 'Please use #add:finalizer: instead'
		transformWith: '`@receiver add: `@object executor: `@executor'
			-> '`@receiver add: `@object finalizer: `@executor'.
	^ self add: anObject finalizer: finalizer
]
