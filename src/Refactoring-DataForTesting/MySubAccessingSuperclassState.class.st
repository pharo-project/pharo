Class {
	#name : 'MySubAccessingSuperclassState',
	#superclass : 'MyClassARoot',
	#category : 'Refactoring-DataForTesting-StaticModel',
	#package : 'Refactoring-DataForTesting',
	#tag : 'StaticModel'
}

{ #category : 'accessing' }
MySubAccessingSuperclassState class >> accessingFromClassSideAndSubclass [

	^ Shared2
]

{ #category : 'accessing' }
MySubAccessingSuperclassState class >> sharedOnlyInClassSideSubclass [

	^ SharedOnlyInClassSideSubclass
]

{ #category : 'accessing' }
MySubAccessingSuperclassState >> accessing [

	"Duplicate here on purpose for testing purposes"
	^ instVarName1
]

{ #category : 'accessing' }
MySubAccessingSuperclassState >> accessing2InSub [

	^ instVarName1 
]

{ #category : 'accessing' }
MySubAccessingSuperclassState >> accessingSharedVariable [

	"Duplicate here on purpose for testing purposes"
	^ Shared1 
]

{ #category : 'accessing' }
MySubAccessingSuperclassState >> accessingSharedVariable2 [

	^ Shared2
]

{ #category : 'actions' }
MySubAccessingSuperclassState >> writing [

	instVarName1 := 42 
]
