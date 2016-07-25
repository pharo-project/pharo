private
detachFromSystem
	example ifNotNil: [ example parents remove: self ifAbsent: [ ] ].
	example := nil