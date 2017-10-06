tests
testSearchString2

	| anchor |
	
	anchor := ReVarSearchSourceAnchor
		entity: (
			Object newAnonymousSubclass
				addClassVarNamed: 'Var';
				yourself)
		string: 'Var'.
		
	self
		assert: anchor searchString
		equals: '''Var'''