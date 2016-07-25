testing
isCompleted
	^ (self leafNodes anySatisfy: [ :node | node pathWeight = Float infinity ]) not