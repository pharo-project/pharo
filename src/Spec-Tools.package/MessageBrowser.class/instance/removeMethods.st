actions
removeMethods
	self currentMethod ifNotNil: [ :method |
		model removeMethod: method inClass: method methodClass ]