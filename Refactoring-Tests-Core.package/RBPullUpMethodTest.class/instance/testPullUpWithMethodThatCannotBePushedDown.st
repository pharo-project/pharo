failure tests
testPullUpWithMethodThatCannotBePushedDown
	model 
		defineClass: 'Object subclass: #SomeClass instanceVariableNames: '''' classVariableNames: ''Foo'' poolDictionaries: '''' category: #''Refactory-Test data'''.
	(model classNamed: #SomeClass) compile: 'yourself ^54'
		classified: #(#accessing).
	self shouldFail: (RBPullUpMethodRefactoring 
				model: model
				pullUp: #(#yourself)
				from: (model classNamed: #SomeClass))