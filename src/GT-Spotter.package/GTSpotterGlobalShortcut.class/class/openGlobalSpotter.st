accessing
openGlobalSpotter
	| spotterModel |
		
	self reset.
	spotterModel := GTSpotter new.
	
	self class environment at: #GTSpotterEventCollector ifPresent: [ :eventCollector |
		eventCollector new
			perform: #occupant: with: spotterModel;
			perform: #category: with: #spotter;
			register;
			perform: #linkToSpotter: with: spotterModel;
			perform: #recordSpotterOpenEvent ].
	
	GTSpotterMorph new
		extent: (World width / 2.4 @ (World height / 1.6)) asIntegerPoint;
		spotterModel: spotterModel;
		doLayout;
		openCenteredInWorld.
		
	spotterModel class markOpened.