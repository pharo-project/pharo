protocol
updateMethodList: aMethodOrNil
	
	self update: #getMethodItem:.
	self selectMethod: aMethodOrNil.
	aMethodOrNil ifNotNil: [  
		| index |
		index := (methods collect: [:each | each selector]) indexOf: aMethodOrNil selector ifAbsent: 0.
		self selectedMethodIndex: index.]