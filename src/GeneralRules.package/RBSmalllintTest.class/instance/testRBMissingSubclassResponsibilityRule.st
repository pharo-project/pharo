tests
testRBMissingSubclassResponsibilityRule
	| sup sub1 sub2 rule |
	rule := RBMissingSubclassResponsibilityRule new.
	sup := Object subclass: 'MySuperclass' instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: self class category.
	sub1 := sup subclass: 'MySubclass1' instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: self class category.
	sub2 := sup subclass: 'MySubclass2' instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: self class category.
	
	sub1 compile: 'method ^ 3'.
	sub2 compile: 'method ^ 5'.
	
	self deny: (rule check: sup) isEmpty.
	
	sup  compile: 'method ^ 2'.
		
	self assert: (rule check: sup) isEmpty.
		
	sup  compile: 'method ^self subclassResponsibility'.
		
	self assert: (rule check: sup) isEmpty.
		
	sub1 removeFromSystem.
	sub2 removeFromSystem.
	sup  removeFromSystem