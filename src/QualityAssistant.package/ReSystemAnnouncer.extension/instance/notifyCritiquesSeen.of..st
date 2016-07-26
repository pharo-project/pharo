*qualityAssistant-announce
notifyCritiquesSeen: critiques of: anEntity

	self announce: (
		QACritiquesSeen new
			critiques: critiques;
			entity: anEntity;
			yourself)
	