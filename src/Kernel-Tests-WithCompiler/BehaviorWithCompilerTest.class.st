Class {
	#name : #BehaviorWithCompilerTest,
	#superclass : #TestCase,
	#instVars : [
		'c1',
		'c2'
	],
	#category : #'Kernel-Tests-WithCompiler'
}

{ #category : #tests }
BehaviorWithCompilerTest >> newClass1 [

	^ self class classInstaller make: [ :aClassBuilder | 
		aClassBuilder 
			name: 'MySuperclass';
			package: self class category ]
]

{ #category : #tests }
BehaviorWithCompilerTest >> newClass2 [

	^ self class classInstaller make: [ :aClassBuilder | 
		aClassBuilder 
			name: 'MySubclass';
			superclass: c1;
			package: self class category ]

]

{ #category : #running }
BehaviorWithCompilerTest >> tearDown [ 
	
	c2 ifNotNil: [ c2 removeFromSystem ].
	c1 ifNotNil: [ c1 removeFromSystem ].
	
	super tearDown.
]

{ #category : #tests }
BehaviorWithCompilerTest >> testCanPerform [
	
	c1 := self newClass1.

	c1 compile: 'method1 ^self subclassResponsibility'.
	c1 compile: 'method2 ^123'.
	
	self deny: (c1 canPerform: #method1).
	self assert: (c1 canPerform: #method2).
	self deny: (c1 canPerform: #method3).
]

{ #category : #tests }
BehaviorWithCompilerTest >> testCanPerformInSubclass [
	
	c1 := self newClass1.
	
	c2 := self newClass2.
	
	c2 compile: 'method1 ^ 3'.
	c2 compile: 'method3 ^5'.
	
	self assert: (c2 canPerform: #method1).
	self assert: (c2 canPerform: #method3).
]

{ #category : #tests }
BehaviorWithCompilerTest >> testCanUnderstand [
	
	c1 := self newClass1.
	c1 compile: 'method1 ^self subclassResponsibility'.
	c1 compile: 'method2 ^123'.
	
	self assert: (c1 canUnderstand: #method1).
	self assert: (c1 canUnderstand: #method2).
	self deny: (c1 canUnderstand: #method3).
]

{ #category : #tests }
BehaviorWithCompilerTest >> testCanUnderstandInSubclass [
	
	c1 := self newClass1.

	c2 := self newClass2.
	c2 compile: 'method1 ^ 3'.
	c2 compile: 'method3 ^5'.
	
	self assert: (c2 canUnderstand: #method1).
	self assert: (c2 canUnderstand: #method3).
]

{ #category : #tests }
BehaviorWithCompilerTest >> testCompile [

	| behavior |

	behavior := Behavior new.
	behavior compile: 'thisIsATest ^2'.
	behavior new thisIsATest
]
