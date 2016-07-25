testing-environments
testEnvironmentWrapper
	| printString wrapper |
	printString := RBBrowserEnvironment new referencesTo: #printString.
	wrapper := RBBrowserEnvironmentWrapper onEnvironment: printString.
	self assert: wrapper numberSelectors = printString numberSelectors.
	self assert: wrapper numberClasses = printString numberClasses.
	self assert: wrapper environment == printString