initialization
initialize
	super initialize.
	self
		replace: 'Smalltalk ui icons `iconName'
		by: [ :node :matchMap |
			RBParser parseExpression:
				'#',
				(matchMap at: #'`iconName'),
				' asIcon' ]