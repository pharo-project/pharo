accessing
enabled: aBoolean

	aBoolean 
		ifTrue: [ self color: self defaultColor ]
		ifFalse: [ self color: self theme disabledTextColor ]