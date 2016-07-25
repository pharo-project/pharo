events
dispatchMorphicEvent: anEvent

	morphicWorld defer: [  		
			morphicWorld activeHand handleEvent: anEvent ]. 
	
	