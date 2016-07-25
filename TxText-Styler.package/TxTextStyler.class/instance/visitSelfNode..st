visiting
visitSelfNode: aSelfNode
	| link methodClass |
	
	self 
		addStyle: #self
		forNode: aSelfNode.
	
	methodClass := aSelfNode methodNode methodClass.
		
	link := TxActionAttribute new 
		filter: [ :evt | evt commandKeyPressed and: [ evt  isMouseDown] ];
		value: [ methodClass browse ];
		yourself.

	self 
		addAttribute: link
		forNode: aSelfNode.