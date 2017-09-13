running
critiqueFor: aMethod withInterval: anInterval

	^ ReTrivialCritique
		withAnchor: (ReIntervalSourceAnchor
			entity: aMethod
			interval: anInterval)
		by: self