*GT-SpotterExtensions-Core
gtAllSubtopics
	 ^ self subtopics flatCollect: [:aTopic |
		aTopic asOrderedCollection, aTopic gtAllSubtopics ]
		 