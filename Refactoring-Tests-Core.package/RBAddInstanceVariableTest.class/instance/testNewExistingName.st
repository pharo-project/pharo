failure tests
testNewExistingName
	| refactoring |
	model := RBNamespace new.
	model name: 'Add instance variable'.
	model 
		defineClass: 'Object subclass: #FOOBAR 
				instanceVariableNames: ''asdf'' 	
				classVariableNames: ''''
				poolDictionaries: ''''
				category: #''Refactory-Test data'''.
	refactoring := RBAddInstanceVariableRefactoring 
				model: model
				variable: 'asdf'
				class: (model classNamed: #FOOBAR).
	self shouldFail: refactoring