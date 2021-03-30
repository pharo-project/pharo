Extension { #name : #Array }

{ #category : #'*NewTools-Inspector-Extensions' }
Array >> displayString [

	^ super displayString contractTo: 200
]

{ #category : #'*NewTools-Inspector-Extensions' }
Array >> gtDisplayString [
	self deprecated: 'Use displayString' transformWith: '`@receiver gtDisplayString' -> '`@receiver displayString'.
	^ self displayString
]
