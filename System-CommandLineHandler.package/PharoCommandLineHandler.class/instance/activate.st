activation
activate

	self isChangingPreferences
		ifTrue: [ self changePreferences ]
		ifFalse: [ self runPreferences ].
		
	^ super activate.