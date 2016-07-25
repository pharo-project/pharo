tests
testPriorityOrderInTheSameCategoryIsRespected
	manager
		registerSystemClassNamed: 'aFirstSubscription' atPriority: 100;
		registerSystemClassNamed: 'aSecondSubscription' atPriority: 110;
		registerSystemClassNamed: 'aThirdSubscriptionThatShouldGoFirst' atPriority: 90.
		

	self 
		assertHandledId: manager startupList 
		equals: #( 'aThirdSubscriptionThatShouldGoFirst' 'aFirstSubscription' 'aSecondSubscription' ).