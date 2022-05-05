Extension { #name : #JobProgressBarMorph }

{ #category : #'*Deprecated10' }
JobProgressBarMorph class >> isInterruptable [

	self
		deprecated: 'Please use #isInterruptible instead'
		transformWith:
		'`@receiver isInterruptable' -> '`@receiver isInterruptible'.

	^ self isInterruptible
]

{ #category : #'*Deprecated10' }
JobProgressBarMorph class >> isInterruptable: aBoolean [

	self
		deprecated: 'Please use #isInterruptible: instead'
		transformWith:
		'`@receiver isInterruptable: `@arg' -> '`@receiver isInterruptible: `@arg'.
		
	self isInterruptible: aBoolean
]
