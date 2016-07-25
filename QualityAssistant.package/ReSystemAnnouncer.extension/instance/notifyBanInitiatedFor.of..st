*qualityAssistant-announce
notifyBanInitiatedFor: aQACritique of: anEntity

	self announce: (
		QAInitiateCritiqueBan new
			critique: aQACritique;
			entity: anEntity;
			yourself)
	