tests
testRBMissingSubclassResponsibilityRule
	| sup sub1 sub2 |
	sup := Object subclass: 'MySuperclass' instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: self class category.
	sub1 := sup subclass: 'MySubclass1' instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: self class category.
	sub2 := sup subclass: 'MySubclass2' instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: self class category.
	
	sub1 compile: 'method ^ 3'.
	sub2 compile: 'method ^ 5'.
	
	self deny: (RBMissingSubclassResponsibilityRule new
		runOnEnvironment: (RBClassEnvironment class: sup);
		result) isEmpty.
	
	sup  compile: 'method ^ 2'.
		
	self assert: (RBMissingSubclassResponsibilityRule new
		runOnEnvironment: (RBClassEnvironment class: sup);
		result) isEmpty.
		
	sup  compile: 'method ^self subclassResponsibility'.
		
	self assert: (RBMissingSubclassResponsibilityRule new
		runOnEnvironment: (RBClassEnvironment class: sup);
		result) isEmpty.
		
	sub1 removeFromSystem.
	sub2 removeFromSystem.
	sup  removeFromSystem