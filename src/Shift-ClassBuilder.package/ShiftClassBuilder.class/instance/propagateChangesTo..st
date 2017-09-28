changes
propagateChangesTo: anotherBuilder
	changes do: [ :e | e propagateToSubclasses: anotherBuilder ]