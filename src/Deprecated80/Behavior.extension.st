Extension { #name : #Behavior }

{ #category : #'*Deprecated80' }
Behavior >> numberOfInstanceVariables [
	self deprecated: 'Use #instSize or `slots size` instead.' transformWith: '`@receiver numberOfInstanceVariables' -> '`@receiver instSize'.
	^ self instSize
]
