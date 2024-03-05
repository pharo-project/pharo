Extension { #name : 'SharedPool' }

{ #category : '*Deprecated12' }
SharedPool class >> bindingsDo: aBlock [
	self
		deprecated:
		'use classVariables do: instead'
		transformWith: '`@receiver bindingsDo: `@arg1' -> '`@receiver classVariables do: `@arg1'.
	^self classVariables do: aBlock
]
