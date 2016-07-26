initialization
initialize
	super initialize.
	localVars := SortedCollection new.
	instVars := SortedCollection new.
	classVars := SortedCollection new.
	includeSelectors := true.
	includeVariables := true.
	listLimit := 100.