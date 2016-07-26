coverage
selectorsToBeTested

	^ ( { self classToBeTested. self classToBeTested class } flatCollect: [:c | c selectors]) 
			difference: self selectorsToBeIgnored