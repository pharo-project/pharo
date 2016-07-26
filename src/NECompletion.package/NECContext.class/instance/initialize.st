initialization
initialize
	super initialize.
	source := String new.
	position := 0.
	recurseCount := 0.
	variables := true.
	selectors := true.
	ranges := OrderedCollection new.
	completionIndex := 0.
	completionToken := nil