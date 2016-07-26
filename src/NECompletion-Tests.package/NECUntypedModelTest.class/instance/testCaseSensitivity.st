tests
testCaseSensitivity
	| model instances |
	self assert: NECPreferences caseSensitive.
	model := NECUntypedModel 
		class: NECTestClass
		temporaries: OrderedCollection new.
	model listLimit: Float infinity.
		
	model narrowWith: 'typesugg'.
	self assert: model isEmpty.
	
	NECPreferences caseSensitive: false.
	model narrowWith: 'typesugg'.
	instances := model entriesOfType: #instVar.
	self assert: instances size == 2.
	self assert: (instances includes: 'typeSuggestingParameter').
	
	model narrowWith: 'dict'.
	self assert: model notEmpty.
	self assert: ((model entriesOfType: #selector) includes: 'Dictionary')