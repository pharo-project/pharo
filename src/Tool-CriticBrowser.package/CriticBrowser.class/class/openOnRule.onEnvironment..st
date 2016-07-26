instance creation
openOnRule: aRule onEnvironment: aEnv

	| cbr |
	cbr :=  self new rules: aRule;
		environment: aEnv;
		removeTestCase: false;
		yourself.

	cbr openWithSpec.
	cbr applyRules. 
	cbr rulesModel  changed:  #listElementAt:.