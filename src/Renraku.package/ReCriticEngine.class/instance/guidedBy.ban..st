banning
guidedBy: anEntity ban: aCritique

	| candidates |
	
	ReSystemAnnouncer uniqueInstance notifyBanInitiatedFor: aCritique of: anEntity.
	
	candidates := anEntity banLevelCandidates.
	
	(UIManager default
		chooseFrom: (candidates collect: #reBanName)
		values: candidates
		title: 'On which level do you want to ban the rule?')
			ifNotNil: [ :entity | entity ban: aCritique ]