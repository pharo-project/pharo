"
We may want to distribute the following tests in classDescription and others.
But this is future work.
"
Class {
	#name : 'LowLevelReflectiveAPITest',
	#superclass : 'TestCase',
	#category : 'Refactoring-Core-Tests-Model',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Model'
}

{ #category : 'tests' }
LowLevelReflectiveAPITest >> testHasMethodAccessingVariable [

	self assert: (MyClassARoot hasMethodAccessingVariable:  (MyClassARoot slotNamed: #instVarName1)).
	self assert: (MySubAccessingSuperclassState hasMethodAccessingVariable:  (MyClassARoot slotNamed: #instVarName1)).
]

{ #category : 'tests' }
LowLevelReflectiveAPITest >> testIsAccessedIn [

	self assert: ((MyClassARoot slotNamed: #instVarName1) isAccessedIn:  MyClassARoot >> #accessing).
	self assert: ((MyClassARoot slotNamed: #instVarName1) isAccessedIn:  MySubAccessingSuperclassState >> #accessing).
	self assert: ((MyClassARoot slotNamed: #instVarName1) isAccessedIn:  MySubAccessingSuperclassState >> #accessing2InSub).
	self assert: ((MyClassARoot slotNamed: #instVarName1) isAccessedIn:  MySubAccessingSuperclassState >> #writing).
		

	
]

{ #category : 'tests' }
LowLevelReflectiveAPITest >> testWhichMethodsAccessSubclassState [

	| accessingMethods accessingSelectors |
	accessingMethods := (MyClassARoot whichMethodsAccess: #instVarName1).
	self assert: accessingMethods size equals: 1.
	self assert: accessingMethods first selector equals: #accessing. 
	self assert: (MyClassARoot whichMethodsAccess: #instVarName1) first methodClass equals: MyClassARoot.
	 
	self assert: (MySubAccessingSuperclassState whichMethodsAccess: #instVarName1) size equals: 3.
	
	accessingMethods := (MySubAccessingSuperclassState whichMethodsAccess: #instVarName1).
	accessingSelectors := accessingMethods collect: [ :each | each selector ].
	
	self assert: (accessingSelectors includes: #accessing).
	self assert: (accessingSelectors includes: #accessing2InSub).
	self assert: (accessingSelectors includes: #writing).
		
	self assert: (accessingMethods select: [ :each | each selector = #accessing2InSub ]) first methodClass equals: MySubAccessingSuperclassState.
	self assert: (accessingMethods select: [ :each | each selector = #accessing ]) first methodClass equals: MySubAccessingSuperclassState.
	self assert: (accessingMethods select: [ :each | each selector = #writing ]) first methodClass equals: MySubAccessingSuperclassState.
]

{ #category : 'tests' }
LowLevelReflectiveAPITest >> testWhichMethodsReadSubclassState [

	| accessingMethods accessingSelectors |
	self assert: (MyClassARoot whichMethodsRead: #instVarName1) first selector equals: #accessing. 
	self assert: (MyClassARoot whichMethodsRead: #instVarName1) first methodClass equals: MyClassARoot. 
	self assert: (MySubAccessingSuperclassState whichMethodsRead: #instVarName1) size equals: 2.
	
	accessingMethods := (MySubAccessingSuperclassState whichMethodsRead: #instVarName1).
	accessingSelectors := accessingMethods collect: [ :each | each selector ].
	
	self assert: (accessingSelectors includes: #accessing).
	self assert: (accessingSelectors includes: #accessing2InSub).
	
	self assert: (accessingMethods select: [ :each | each selector =  #accessing2InSub ]) first methodClass equals: MySubAccessingSuperclassState.
	self assert: (accessingMethods select: [ :each | each selector =  #accessing ]) first methodClass equals: MySubAccessingSuperclassState.
	
	
]

{ #category : 'tests' }
LowLevelReflectiveAPITest >> testWhichMethodsWriteSubclassState [

	self assert: (MyClassARoot whichMethodsWrite: #instVarName1) isEmpty. 
	self assert: (MySubAccessingSuperclassState whichMethodsWrite: #instVarName1) size equals: 1.
	self assert: (MySubAccessingSuperclassState whichMethodsWrite: #instVarName1) first selector equals: #writing.
	self assert: (MySubAccessingSuperclassState whichMethodsWrite: #instVarName1) first methodClass equals: MySubAccessingSuperclassState.
	
	
]

{ #category : 'tests' }
LowLevelReflectiveAPITest >> testWhichSelectorsAccessSubclassState [

	self assert: (MyClassARoot whichSelectorsAccess: #instVarName1) equals: #(accessing). 
	self assert: (MySubAccessingSuperclassState whichSelectorsAccess: #instVarName1) asSet equals: #(#accessing #accessing2InSub #writing) asSet
]
