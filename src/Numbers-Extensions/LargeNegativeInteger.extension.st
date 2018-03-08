Extension { #name : #LargeNegativeInteger }

{ #category : #'*Numbers-Extensions' }
LargeNegativeInteger >> ln [
	^DomainError signal: 'ln is only defined for x > 0' from: 0
]

{ #category : #'*Numbers-Extensions' }
LargeNegativeInteger >> log [
	^DomainError signal: 'log is only defined for x > 0' from: 0
]

{ #category : #'*Numbers-Extensions' }
LargeNegativeInteger >> sqrt [
	"Answer the square root of the receiver."
	^ DomainError signal: 'sqrt undefined for number less than zero.'
]
