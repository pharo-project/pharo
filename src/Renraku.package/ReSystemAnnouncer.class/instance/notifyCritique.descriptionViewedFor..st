announce
notifyCritique: aCritique descriptionViewedFor: anEntity

	self announce: (
		ReCritiqueDescriptionViewed new 
			critique: aCritique;
			entity: anEntity;
			yourself)
  