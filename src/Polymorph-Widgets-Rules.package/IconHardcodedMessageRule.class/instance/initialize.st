initialization
initialize
	super initialize.
	self
		replace: 'Smalltalk ui icons `iconName'
		byEvaluating: [ :node :matchMap | 
			RBParser
				parseExpression: '#' , (matchMap at: #'`iconName') , ' asIcon' ]