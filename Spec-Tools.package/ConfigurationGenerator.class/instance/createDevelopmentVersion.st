metacello
createDevelopmentVersion
	self hasAnyBaseline 
		ifFalse: [ UIManager default inform: 'Please, define a baseline first'. ^ self ].
	^ self createVersionForBlessing: #development