initialization
labelString
	reference ifNil: [ ^ '[]' ].
	^ baseLabel, reference basename contractTo: 50