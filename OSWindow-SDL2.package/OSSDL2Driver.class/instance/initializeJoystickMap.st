window creation and deletion
initializeJoystickMap

	JoystickMap ifNil: [ 
		JoystickMap := Dictionary new. 
	].