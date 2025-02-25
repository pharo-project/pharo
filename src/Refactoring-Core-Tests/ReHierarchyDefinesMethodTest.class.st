Class {
	#name : 'ReHierarchyDefinesMethodTest',
	#superclass : 'TestCase',
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'tests' }
ReHierarchyDefinesMethodTest >> model [

	^ RBNamespace onEnvironment:
		  (RBClassEnvironment classes: {MyClassARoot . MySubAccessingSuperclassState} )
]

{ #category : 'tests' }
ReHierarchyDefinesMethodTest >> testHierarchyDefinesByCheckingInRootDefiningTheMethod [

	| myClassARoot hier |
	myClassARoot := self model classNamed: #MyClassARoot.
	
	hier := ReHierarchyDefinesMethod new 
		class: myClassARoot; selector: #accessing.
		
	" the class MyClassARoot defines the method #accessing "
	self assert: hier check. 
	
	self assert: hier violators isNotEmpty.
	self assert: hier violators size equals: 1
]

{ #category : 'tests' }
ReHierarchyDefinesMethodTest >> testHierarchyDefinesByCheckingInRootNotDefiningTheMethod [

	| myClassARoot hier |
	myClassARoot := self model classNamed: #MyClassARoot.
	
	hier := ReHierarchyDefinesMethod new 
		class: myClassARoot; selector: #accessingSharedVariable2.
		
	" the subclass of MyClassARoot defines the method #accessingSharedVariable2 "
	self assert: hier check. 
	
	self assert: hier violators isNotEmpty.
	self assert: hier violators size equals: 1
]

{ #category : 'tests' }
ReHierarchyDefinesMethodTest >> testHierarchyDefinesByCheckingInSubclassDefiningTheMethod [

	| myClassARoot hier |
	myClassARoot := self model classNamed: #MySubAccessingSuperclassState.
	
	hier := ReHierarchyDefinesMethod new 
		class: myClassARoot; selector: #accessing.
		
	" the class MySubAccessingSuperclassState defines too the method #accessing "
	self assert: hier check. 
	
	self assert: hier violators isNotEmpty.
	self assert: hier violators size equals: 1
]

{ #category : 'tests' }
ReHierarchyDefinesMethodTest >> testHierarchyDefinesByCheckingInSubclassNotDefiningTheMethod [

	| myClassARoot hier |
	myClassARoot := self model classNamed: #MySubAccessingSuperclassState.
	
	hier := ReHierarchyDefinesMethod new 
		class: myClassARoot; selector: #initialize.
		
	" the superclass of MySubAccessingSuperclassState defines the method #initialize "
	self assert: hier check. 
	
	self assert: hier violators isNotEmpty.
	self assert: hier violators size equals: 1
]
