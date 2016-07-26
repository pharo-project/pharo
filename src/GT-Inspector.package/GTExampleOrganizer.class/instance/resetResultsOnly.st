initializing
resetResultsOnly
	" this is expensive, but thorough and makes sure the gc will really get rid of all gtexample-result-related objects "
	GTExampleResult allSubInstances do: [ :each | [ each detachFromSystem ] on: Error do: [ " ignore / do not abort " ] ].