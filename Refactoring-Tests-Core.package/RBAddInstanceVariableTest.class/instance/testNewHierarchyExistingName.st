failure tests
testNewHierarchyExistingName
	| refactoring |
	model := RBNamespace new.
	model name: 'Add instance variable'.
	model 
		defineClass: 'Object subclass: #FOOBAR 
				instanceVariableNames: ''asdf'' 	
				classVariableNames: ''''
				poolDictionaries: ''''
				category: #''Refactory-Test data'''.
	model 
		defineClass: 'FOOBAR subclass: #BARFOO 
				instanceVariableNames: '''' 	
				classVariableNames: ''''
				poolDictionaries: ''''
				category: ''Refactory-Test data'''.
	refactoring := RBAddInstanceVariableRefactoring 
				model: model
				variable: 'asdf'
				class: (model classNamed: #BARFOO).
	self shouldFail: refactoring