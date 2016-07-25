testing
testSystemIntegrity
	| classes environment |
	classes := IdentitySet new.
	environment := RBBrowserEnvironment new.
	environment allClassesDo: [ :class |
		(classes includes: class)
			ifFalse: [ classes add: class ]
			ifTrue: [ self error: 'The class ' , class name , ' that is available under two or more names. This causes problems with the refactoring browser.' ] ]