instance creation
openOnWorkingConfiguration: aWorkingConfiguration

	| cbr |
	cbr :=  self new 
		rules: aWorkingConfiguration rule;
		environment: aWorkingConfiguration environment;
		removeTestCase: aWorkingConfiguration removeTestCase;
		yourself.

	cbr openWithSpec.
	cbr applyRules. 
	cbr rulesModel  changed:  #listElementAt:.