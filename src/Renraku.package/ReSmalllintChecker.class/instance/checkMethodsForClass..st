private
checkMethodsForClass: aClass
	environment
		selectorsForClass: aClass
		do: [ :selector | | method |
			method := aClass>>selector.
			self getCritiquesAbout: method by: methodRules.
			method ast nodesDo: [ :node |
				nodeRules do: [ :r |
					r
						check: node
						forCritiquesDo: [ :crit |
							crit sourceAnchor initializeEnitity: method.
							self addCritique: crit ] ] ] ]