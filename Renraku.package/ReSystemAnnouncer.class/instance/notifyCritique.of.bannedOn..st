announce
notifyCritique: aCritique of: anEntity bannedOn: anEntityBanLevel

	self announce: (
		ReCritiqueBanned new
			critique: aCritique;
			entity: anEntity;
			entityBanLevel: anEntityBanLevel;
			yourself)
