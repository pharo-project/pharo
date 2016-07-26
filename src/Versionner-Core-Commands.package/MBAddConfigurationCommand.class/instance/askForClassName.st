execute
askForClassName
	^ self uiManager
		request: 'Name of the new Metacello configuration (e.g., ConfigurationOfYourSoftware)' translated
		initialAnswer: 'ConfigurationOf'