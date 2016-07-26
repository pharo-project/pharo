handling
outer
	"Evaluate the enclosing exception action and return to here instead of signal if it resumes (see #resumeUnchecked:)."

	| prevOuterContext |
	self isResumable 
		ifTrue: [
			prevOuterContext := outerContext.
			outerContext := thisContext contextTag ].
	self pass.
