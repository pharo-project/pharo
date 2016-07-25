event subscriptions
subscribesResetEnvironmentOn: aFinder to: anInstance 
	aFinder
		when: #resetEnvironment
		send: #resetEnvironment
		to: anInstance