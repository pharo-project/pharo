caching
releaseCachedState

	super releaseCachedState.
	filledForm := nil.
	borderForm := nil.
	curveState := nil.
	(self hasProperty: #flex) ifTrue:
		[self removeProperty: #flex].
