cleanup
cleanUp: aggressive
	"Dump all ActionMaps but only when we're aggressively cleaning"

	aggressive ifTrue: [ActionMaps := nil].