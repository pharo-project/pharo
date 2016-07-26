private
criticsModelContains: aCritic

	^ criticsModel listItems anySatisfy: [ :each |
			 each name = aCritic name]