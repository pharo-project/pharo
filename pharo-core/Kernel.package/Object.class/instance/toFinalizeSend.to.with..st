finalization
toFinalizeSend: aSelector to: aFinalizer with: aResourceHandle
	"When I am finalized (e.g., garbage collected) close the associated resource handle by sending aSelector to the appropriate finalizer (the guy who knows how to get rid of the resource).
	WARNING: Neither the finalizer nor the resource handle are allowed to reference me. If they do, then I will NEVER be garbage collected. Since this cannot be validated here, it is up to the client to make sure this invariant is not broken."
	self == aFinalizer ifTrue:[self error: 'I cannot finalize myself'].
	self == aResourceHandle ifTrue:[self error: 'I cannot finalize myself'].
	^self finalizationRegistry add: self executor:
		(ObjectFinalizer new
			receiver: aFinalizer
			selector: aSelector
			argument: aResourceHandle)