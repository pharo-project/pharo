caching
releaseCachedState

	super releaseCachedState.
	filledForm := nil.
	arrowForms := nil.
	borderForm := nil.
	curveState := nil.
	(self hasProperty: #flex) ifTrue:
		[self removeProperty: #flex].
