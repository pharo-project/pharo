Class {
	#name : 'REHierarchyDefinesMethodTest',
	#superclass : 'TestCase',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'tests' }
REHierarchyDefinesMethodTest >> model [

	^ RBNamespace onEnvironment:
		  (RBClassEnvironment classes: {MyClassARoot . MySubAccessingSuperclassState} )
]

{ #category : 'as yet unclassified' }
REHierarchyDefinesMethodTest >> testHierarchyDefinesByCheckingInRootDefiningTheMethod [

	| myClassARoot hier |
	myClassARoot := self model classNamed: #MyClassARoot.
	
	hier := REHierarchyDefinesMethod new 
		class: myClassARoot; selector: #accessing.
		
	" the class MyClassARoot defines the method #accessing "
	self assert: hier check. 
	
	self assert: hier violators isNotEmpty.
	self assert: hier violators size equals: 1
]

{ #category : 'as yet unclassified' }
REHierarchyDefinesMethodTest >> testHierarchyDefinesByCheckingInRootNotDefiningTheMethod [

	| myClassARoot hier |
	myClassARoot := self model classNamed: #MyClassARoot.
	
	hier := REHierarchyDefinesMethod new 
		class: myClassARoot; selector: #accessingSharedVariable2.
		
	" the subclass of MyClassARoot defines the method #accessingSharedVariable2 "
	self assert: hier check. 
	
	self assert: hier violators isNotEmpty.
	self assert: hier violators size equals: 1
]

{ #category : 'as yet unclassified' }
REHierarchyDefinesMethodTest >> testHierarchyDefinesByCheckingInSubclassDefiningTheMethod [

	| myClassARoot hier |
	myClassARoot := self model classNamed: #MySubAccessingSuperclassState.
	
	hier := REHierarchyDefinesMethod new 
		class: myClassARoot; selector: #accessing.
		
	" the class MySubAccessingSuperclassState defines too the method #accessing "
	self assert: hier check. 
	
	self assert: hier violators isNotEmpty.
	self assert: hier violators size equals: 1
]

{ #category : 'as yet unclassified' }
REHierarchyDefinesMethodTest >> testHierarchyDefinesByCheckingInSubclassNotDefiningTheMethod [

	| myClassARoot hier |
	myClassARoot := self model classNamed: #MySubAccessingSuperclassState.
	
	hier := REHierarchyDefinesMethod new 
		class: myClassARoot; selector: #initialize.
		
	" the superclass of MySubAccessingSuperclassState defines the method #initialize "
	self assert: hier check. 
	
	self assert: hier violators isNotEmpty.
	self assert: hier violators size equals: 1
]
