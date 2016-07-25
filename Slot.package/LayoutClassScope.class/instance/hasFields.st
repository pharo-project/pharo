testing
hasFields
	self do: [ :slot | 
		slot size > 0 
			ifTrue: [ ^ true ]].
	^ parentScope hasFields