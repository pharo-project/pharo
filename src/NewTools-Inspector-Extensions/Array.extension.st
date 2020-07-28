Extension { #name : #Array }

{ #category : #'*NewTools-Inspector-Extensions' }
Array >> gtDisplayString [

	^ super gtDisplayString contractTo: 200
]
