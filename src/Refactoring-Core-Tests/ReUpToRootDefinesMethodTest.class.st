Class {
	#name : 'ReUpToRootDefinesMethodTest',
	#superclass : 'TestCase',
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'tests' }
ReUpToRootDefinesMethodTest >> model [

	^ RBNamespace onEnvironment:
		  (RBClassEnvironment classes: {MyClassARoot . MySubAccessingSuperclassState} )
]

{ #category : 'tests' }
ReUpToRootDefinesMethodTest >> testUpToRootDefinesByCheckingInRootDefiningTheMethod [

	| myClassARoot hier |
	myClassARoot := self model classNamed: #MyClassARoot.
	
	hier := ReUpToRootDefinesMethod new 
		class: myClassARoot; selector: #accessing.
		
	" the class MyClassARoot defines the method #accessing "
	self assert: hier check. 
	
	self assert: hier violators isEmpty.
	
	hier := ReUpToRootDoesNotDefinesMethod new 
		class: myClassARoot; selector: #accessing.
		
	" the class MyClassARoot defines the method #accessing "
	self deny: hier check. 
	
	self assert: hier violators isNotEmpty.
	self assert: hier violators size equals: 1
]

{ #category : 'tests' }
ReUpToRootDefinesMethodTest >> testUpToRootDefinesByCheckingInSubclassDefiningTheMethod [

	| myClassARoot hier |
	myClassARoot := self model classNamed: #MySubAccessingSuperclassState.
	
	hier := ReUpToRootDefinesMethod new 
		class: myClassARoot; selector: #accessing.
		
	" the class MySubAccessingSuperclassState defines the method #accessing "
	self assert: hier check. 
	
	self assert: hier violators isEmpty.
	
	hier := ReUpToRootDoesNotDefinesMethod new 
		class: myClassARoot; selector: #accessing.
		
	" the class MySubAccessingSuperclassState defines the method #accessing "
	self deny: hier check. 
	
	self assert: hier violators isNotEmpty.
	self assert: hier violators size equals: 1
]

{ #category : 'tests' }
ReUpToRootDefinesMethodTest >> testUpToRootDefinesByCheckingInSubclassNotDefiningTheMethod [

	| myClassARoot hier |
	myClassARoot := self model classNamed: #MySubAccessingSuperclassState.
	
	hier := ReUpToRootDefinesMethod new 
		class: myClassARoot; selector: #initialize.
		
	" the superclass of MySubAccessingSuperclassState defines the method #initialize "
	self assert: hier check. 

	self assert: hier violators isEmpty.
	
	hier := ReUpToRootDoesNotDefinesMethod new 
		class: myClassARoot; selector: #initialize.
		
	" the superclass of MySubAccessingSuperclassState defines the method #initialize "
	self deny: hier check. 

	self assert: hier violators isNotEmpty
]

{ #category : 'tests' }
ReUpToRootDefinesMethodTest >> testUpToRootNotDefineTheMethod [

	| myClassARoot hier |
	myClassARoot := self model classNamed: #MyClassARoot.
	
	hier := ReUpToRootDefinesMethod new 
		class: myClassARoot; selector: #accessingSharedVariable2.
		
	" the subclass of MyClassARoot defines the method #accessingSharedVariable2 but we do not look in it.
	andnMyClassARoot does not define it."
	self deny: hier check. 
	
	self assert: hier violators size equals: 1.
	
	hier := ReUpToRootDoesNotDefinesMethod new 
		class: myClassARoot; selector: #accessingSharedVariable2.
		
	self assert: hier check. 
	
	self assert: hier violators isEmpty.
]

{ #category : 'tests' }
ReUpToRootDefinesMethodTest >> testUpToRootNotDefineTheMethodStartingFromSubclass [

	| myClassARoot hier |
	myClassARoot := self model classNamed: #MySubAccessingSuperclassState.
	
	hier := ReUpToRootDefinesMethod new 
		class: myClassARoot; selector: #doesNotExist.
		
	" the subclass of MyClassARoot defines the method #accessingSharedVariable2 but we do not look in it.
	andnMyClassARoot does not define it."
	self deny: hier check. 
	
	self assert: hier violators size equals: 1.
	
	hier := ReUpToRootDoesNotDefinesMethod new 
		class: myClassARoot; selector: #doesNotExist.
		
	self assert: hier check. 
	
	self assert: hier violators isEmpty
]
