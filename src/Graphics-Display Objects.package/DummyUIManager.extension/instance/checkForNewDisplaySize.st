*Graphics-Display Objects
checkForNewDisplaySize

	Display extent = DisplayScreen actualScreenSize ifTrue: [^ self].
	DisplayScreen startUp.
