as yet unclassified
getCritiquesAbout: anEntity by: rules

	rules do: [ :r |
		r
			check: anEntity
			forCritiquesDo: [ :crit |
				critiques add: crit ]  ]