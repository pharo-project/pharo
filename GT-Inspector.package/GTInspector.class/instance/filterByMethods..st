context
filterByMethods: aCollection
	| filter |
	
	filter := GTInspectorMethodListFilter new.
	aCollection  do: [ :aMethod |
		filter addAndSelectSignatureFor: aMethod ].
	self presentationFilter: filter
		