running-helpers
critiqueFor: aMethod about: aVarNode

	^ (ReTrivialCritique
		withAnchor: (ReIntervalSourceAnchor
			entity: aMethod
			interval: aVarNode sourceInterval)
		by: self)
		tinyHint: aVarNode name;
		yourself