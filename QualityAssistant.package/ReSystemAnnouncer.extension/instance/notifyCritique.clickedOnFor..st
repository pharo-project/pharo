*qualityAssistant-announce
notifyCritique: aCritique clickedOnFor: anEntity 
	
	self announce: (
		QACritiqueClickedOn new 
			critique: aCritique;
			entity: anEntity;
			yourself)