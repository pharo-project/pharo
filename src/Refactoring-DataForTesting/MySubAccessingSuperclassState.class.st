Class {
	#name : 'MySubAccessingSuperclassState',
	#superclass : 'MyClassARoot',
	#category : 'Refactoring-DataForTesting-StaticModel',
	#package : 'Refactoring-DataForTesting',
	#tag : 'StaticModel'
}

{ #category : 'accessing' }
MySubAccessingSuperclassState >> accessing [

	^ instVarName1  
]

{ #category : 'accessing' }
MySubAccessingSuperclassState >> accessing2InSub [

	^ instVarName1 
]

{ #category : 'actions' }
MySubAccessingSuperclassState >> writing [

	instVarName1 := 42 
]
