actions
ban: aCritique
	
	ReSystemAnnouncer uniqueInstance
		notifyCritique: aCritique
		of: aCritique sourceAnchor entity
		bannedOn: self.

	(aCritique rule isKindOf: ReAbstractRule)
		ifTrue: [
			aCritique rule class enabled: false ]