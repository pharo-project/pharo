instance creation
name: nameOfItem code: code runOnce: aBoolean

	^ self new
		code: code;
		name: nameOfItem;
		runOnce: aBoolean;
		yourself