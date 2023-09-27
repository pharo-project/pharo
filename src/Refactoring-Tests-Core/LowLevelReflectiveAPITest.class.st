"
We may want to distribute the following tests in classDescription and others.
But this is future work.
"
Class {
	#name : 'LowLevelReflectiveAPITest',
	#superclass : 'TestCase',
	#category : 'Refactoring-Tests-Core',
	#package : 'Refactoring-Tests-Core'
}

{ #category : 'tests' }
LowLevelReflectiveAPITest >> testIsAccessedIn [

	self assert: ((MyClassARoot slotNamed: #instVarName1) isAccessedIn:  MyClassARoot >> #accessing).
	self assert: ((MyClassARoot slotNamed: #instVarName1) isAccessedIn:  MySubAccessingSuperclassState >> #accessing).
	self assert: ((MyClassARoot slotNamed: #instVarName1) isAccessedIn:  MySubAccessingSuperclassState >> #accessing2InSub).
	self assert: ((MyClassARoot slotNamed: #instVarName1) isAccessedIn:  MySubAccessingSuperclassState >> #writing).
		

	
]

{ #category : 'tests' }
LowLevelReflectiveAPITest >> testWhichMethodsAccessSubclassState [

	self assert: (MyClassARoot whichMethodsAccess: #instVarName1) first selector equals: #accessing. 
	self assert: (MyClassARoot whichMethodsAccess: #instVarName1) first methodClass equals: MyClassARoot. 
	self assert: (MySubAccessingSuperclassState whichMethodsAccess: #instVarName1) size equals: 3.
	self assert: (MySubAccessingSuperclassState whichMethodsAccess: #instVarName1) first selector equals: #accessing.
	self assert: (MySubAccessingSuperclassState whichMethodsAccess: #instVarName1) first methodClass equals: MySubAccessingSuperclassState.
	self assert: (MySubAccessingSuperclassState whichMethodsAccess: #instVarName1) second selector equals: #accessing2InSub.
	self assert: (MySubAccessingSuperclassState whichMethodsAccess: #instVarName1) third selector equals: #writing.
	
]

{ #category : 'tests' }
LowLevelReflectiveAPITest >> testWhichMethodsReadSubclassState [

	self assert: (MyClassARoot whichMethodsRead: #instVarName1) first selector equals: #accessing. 
	self assert: (MyClassARoot whichMethodsRead: #instVarName1) first methodClass equals: MyClassARoot. 
	self assert: (MySubAccessingSuperclassState whichMethodsRead: #instVarName1) size equals: 2.
	self assert: (MySubAccessingSuperclassState whichMethodsRead: #instVarName1) first selector equals: #accessing.
	self assert: (MySubAccessingSuperclassState whichMethodsRead: #instVarName1) first methodClass equals: MySubAccessingSuperclassState.
	self assert: (MySubAccessingSuperclassState whichMethodsRead: #instVarName1) second selector equals: #accessing2InSub.
	
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
	self assert: (MySubAccessingSuperclassState whichSelectorsAccess: #instVarName1) equals: #(#accessing #accessing2InSub #writing)
]
