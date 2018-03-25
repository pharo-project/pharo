Extension { #name : #LargeNegativeInteger }

{ #category : #'*Math-Operations-Extensions' }
LargeNegativeInteger >> ln [
	^DomainError signal: 'ln is only defined for x > 0' from: 0
]

{ #category : #'*Math-Operations-Extensions' }
LargeNegativeInteger >> log [
	^DomainError signal: 'log is only defined for x > 0' from: 0
]

{ #category : #'*Math-Operations-Extensions' }
LargeNegativeInteger >> sqrt [
	"Answer the square root of the receiver."
	^ DomainError signal: 'sqrt undefined for number less than zero.'
]
