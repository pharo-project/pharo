private
getCritiquesAbout: anEntity by: rules

	rules do: [ :r |
		[ r
			check: anEntity
			forCritiquesDo: [ :crit |
				self addCritique: crit ] ]
			
			on: Error
			do: [ :er | ReSettings showErrors ifTrue: [ er pass ]  ] ]