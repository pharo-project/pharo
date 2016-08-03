api
nextAction

	self delete..
	
	CriticWorkingConfiguration current
		rule: selectedRules;
		environment: environment;
		removeTestCase: removeTestCase.
	CriticBrowser openOnWorkingConfiguration: CriticWorkingConfiguration current