tests
testModelRemoveInstanceVariable
	| class |
	model 
		defineClass: 'nil subclass: #Object instanceVariableNames: ''foo1'' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	class := model classNamed: #Object.
	self assert: (class definesInstanceVariable: 'foo1').
	self executeRefactoring: (RBRemoveInstanceVariableRefactoring 
				model: model
				variable: 'foo1'
				class: class).
	self deny: (class definesInstanceVariable: 'foo1')