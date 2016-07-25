cleanup
reset
	
	(World findA: GTSpotterMorph) ifNotNil: [ :aSpotterMorph | aSpotterMorph spotterModel exitDueTo: #reset ]