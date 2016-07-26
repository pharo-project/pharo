accessing
isTerminated

	process ifNil: [ ^ true ].
	^ process isTerminated