initialize
limit: newLimit
	access critical: [ 
		limit := newLimit.
		limit = 0 ifTrue: [ ^ resources := nil ].
		resources ifNotNil: [ resources removeAll ] ]