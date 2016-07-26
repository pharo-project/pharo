private
sizes
	sizes ~~ nil
		ifFalse: [ sizes := self new computeSizes ].
	^ sizes