private
criticsModelContains: aCritic

	^ criticsModel listItems anySatisfy: [ :each |
			 each sourceAnchor entity name = aCritic name]