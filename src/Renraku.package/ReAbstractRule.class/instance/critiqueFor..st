helpers
critiqueFor: anEntity

	^ ReTrivialCritic
		withAnchor: (self anchorFor: anEntity)
		by: self