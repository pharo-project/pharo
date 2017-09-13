running
check: aMethod forCritiquesDo: aCriticBlock

	aMethod messages
		select: [ :each | each isSelectorSymbol not ]
		thenDo: [ :each |
			aCriticBlock cull: (ReTrivialCritique
				withAnchor: (ReSearchStringSourceAnchor
					entity: aMethod
					string: each)
				by: self
				hint: each) ]
		