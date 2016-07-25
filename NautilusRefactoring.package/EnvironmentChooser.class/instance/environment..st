accessing
environment: anEnvironment
	self environment = anEnvironment 
		ifFalse: [ environmentHolder value: anEnvironment ]