testing-environments
testSelectorEnvironment
	| printString |
	printString := RBBrowserEnvironment new referencesTo: #printString.
	self universalTestFor: printString.
	self deny: universalEnvironment isSelectorEnvironment.
	self assert: printString isSelectorEnvironment.
	self assert: printString numberSelectors = (printString referencesTo: #printString) numberSelectors.
	self assert: printString numberClasses = (printString referencesTo: #printString) numberClasses