Class {
	#name : 'REUpToRootDefinesMethodTest',
	#superclass : 'TestCase',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'tests' }
REUpToRootDefinesMethodTest >> model [

	^ RBNamespace onEnvironment:
		  (RBClassEnvironment classes: {MyClassARoot . MySubAccessingSuperclassState} )
]

{ #category : 'tests' }
REUpToRootDefinesMethodTest >> testUpToRootDefinesByCheckingInRootDefiningTheMethod [

	| myClassARoot hier |
	myClassARoot := self model classNamed: #MyClassARoot.
	
	hier := REUpToRootDefinesMethod new 
		class: myClassARoot; selector: #accessing.
		
	" the class MyClassARoot defines the method #accessing "
	self assert: hier check. 
	
	self assert: hier violators isNotEmpty.
	self assert: hier violators size equals: 1
]

{ #category : 'tests' }
REUpToRootDefinesMethodTest >> testUpToRootDefinesByCheckingInSubclassDefiningTheMethod [

	| myClassARoot hier |
	myClassARoot := self model classNamed: #MySubAccessingSuperclassState.
	
	hier := REUpToRootDefinesMethod new 
		class: myClassARoot; selector: #accessing.
		
	" the class MySubAccessingSuperclassState defines the method #accessing "
	self assert: hier check. 
	
	self assert: hier violators isNotEmpty.
	self assert: hier violators size equals: 1
]

{ #category : 'tests' }
REUpToRootDefinesMethodTest >> testUpToRootDefinesByCheckingInSubclassNotDefiningTheMethod [

	| myClassARoot hier |
	myClassARoot := self model classNamed: #MySubAccessingSuperclassState.
	
	hier := REHierarchyDefinesMethod new 
		class: myClassARoot; selector: #initialize.
		
	" the superclass of MySubAccessingSuperclassState defines the method #initialize "
	self assert: hier check. 
	
	self assert: hier violators isNotEmpty.
	self assert: hier violators size equals: 1
]

{ #category : 'tests' }
REUpToRootDefinesMethodTest >> testUpToRootNotDefineTheMethod [

	| myClassARoot hier |
	myClassARoot := self model classNamed: #MyClassARoot.
	
	hier := REUpToRootDefinesMethod new 
		class: myClassARoot; selector: #accessingSharedVariable2.
		
	" the subclass of MyClassARoot defines the method #accessingSharedVariable2 but we do not look in it.
	andnMyClassARoot does not define it."
	self deny: hier check. 
	
	self assert: hier violators isEmpty.
	
]

{ #category : 'tests' }
REUpToRootDefinesMethodTest >> testUpToRootNotDefineTheMethodStartingFromSubclass [

	| myClassARoot hier |
	myClassARoot := self model classNamed: #MySubAccessingSuperclassState.
	
	hier := REUpToRootDefinesMethod new 
		class: myClassARoot; selector: #doesNotExist.
		
	" the subclass of MyClassARoot defines the method #accessingSharedVariable2 but we do not look in it.
	andnMyClassARoot does not define it."
	self deny: hier check. 
	
	self assert: hier violators isEmpty.
	
]
