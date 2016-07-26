filtering
filterByMethods: aCollection
	| filter |
	
	filter := GTSpotterMethodListProcessorFilter new.
	aCollection  do: [ :aMethod |
		filter allowProcessorsFrom: aMethod ].
	self processorsFilter: filter
		