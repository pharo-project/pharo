Extension { #name : #BehaviorTest }

{ #category : #'*Kernel-Tests-WithCompiler' }
BehaviorTest >> testCanPerform [
	
	| c1 c2 |

	c1 := Object subclass: 'MySuperclass' instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' package: self class category.
	c1 compile: 'method1 ^self subclassResponsibility'.
	c1 compile: 'method2 ^123'.
	
	self deny: (c1 canPerform: #method1).
	self assert: (c1 canPerform: #method2).
	self deny: (c1 canPerform: #method3).
	
	c2 := c1 subclass: 'MySubclass' instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: self class category.
	c2 compile: 'method1 ^ 3'.
	c2 compile: 'method3 ^5'.
	
	self assert: (c2 canPerform: #method1).
	self assert: (c2 canPerform: #method3).
	
	c2 removeFromSystem.
	c1 removeFromSystem.
]

{ #category : #'*Kernel-Tests-WithCompiler' }
BehaviorTest >> testCanUnderstand [
	
	| c1 c2 |

	c1 := Object subclass: 'MySuperclass' instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' package: self class category.
	c1 compile: 'method1 ^self subclassResponsibility'.
	c1 compile: 'method2 ^123'.
	
	self assert: (c1 canUnderstand: #method1).
	self assert: (c1 canUnderstand: #method2).
	self deny: (c1 canUnderstand: #method3).
	
	c2 := c1 subclass: 'MySubclass' instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: self class category.
	c2 compile: 'method1 ^ 3'.
	c2 compile: 'method3 ^5'.
	
	self assert: (c2 canUnderstand: #method1).
	self assert: (c2 canUnderstand: #method3).
	
	c2 removeFromSystem.
	c1 removeFromSystem.
]

{ #category : #'*Kernel-Tests-WithCompiler' }
BehaviorTest >> testCompile [
	| behavior |
	behavior := Behavior new.
	behavior compile: 'thisIsATest ^2'.
	behavior new thisIsATest
]
