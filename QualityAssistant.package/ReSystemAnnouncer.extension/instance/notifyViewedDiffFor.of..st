*qualityAssistant-announce
notifyViewedDiffFor: aQACritique of: anEntity

	self announce: (
		QAViewedDiff new
			critique: aQACritique;
			entity: anEntity;
			yourself)
	