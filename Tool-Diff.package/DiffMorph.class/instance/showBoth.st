accessing
showBoth

	showOnlyDestination ifTrue: [ showOnlyDestination := false ].
	showOnlySource ifTrue: [ showOnlySource := false ].
	
	self updateMorphs