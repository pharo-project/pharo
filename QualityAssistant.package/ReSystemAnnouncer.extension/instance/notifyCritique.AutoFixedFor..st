*qualityAssistant-announce
notifyCritique: aCritique AutoFixedFor: anEntity

	self announce: (
		ReCritiqueAutofixed new
			critique: aCritique;
			entity: anEntity;
			yourself)