accessing
instanceButtonHelpTextFor: buttonStateBoolean

	^ buttonStateBoolean 
		ifFalse: [ 'Show the class side' ]
		ifTrue: [ 'Show the instance side' ]
