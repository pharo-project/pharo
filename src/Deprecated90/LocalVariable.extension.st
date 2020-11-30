Extension { #name : #LocalVariable }

{ #category : #'*Deprecated90' }
LocalVariable >> isUnused [
	self
		deprecated: 'Please use #isUsed not instead'
		transformWith: '`@receiver isUnused' -> '`@receiver isUsed not'.

	^ self isUsed not
]
