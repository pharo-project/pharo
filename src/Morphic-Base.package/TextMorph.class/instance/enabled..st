accessing
enabled: aBoolean

	aBoolean 
		ifTrue: [ text makeAllColor: self defaultColor ]
		ifFalse: [ text makeAllColor: self theme disabledTextColor ].
		
	self changed