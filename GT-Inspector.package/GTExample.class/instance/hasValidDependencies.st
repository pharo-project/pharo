testing
hasValidDependencies
	[ self dependencies ]
		on: Error
		do: [ ^ false ].
	^ true