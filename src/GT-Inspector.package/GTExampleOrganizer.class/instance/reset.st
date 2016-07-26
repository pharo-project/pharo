initializing
reset
	" this is expensive, but thorough and makes sure the gc will really get rid of all gtexample-related objects "
	examples := WeakIdentityKeyDictionary new.
	GTExample allSubInstances do: [ :each | [ each detachFromSystem ] on: Error do: [ " ignore / do not abort " ] ].
	GTExampleProxy allSubInstances do: [ :each | [ each detachFromSystem ] on: Error do: [ " ignore / do not abort " ] ].
	self resetResultsOnly.