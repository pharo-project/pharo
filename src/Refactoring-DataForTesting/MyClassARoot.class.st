Class {
	#name : 'MyClassARoot',
	#superclass : 'Object',
	#instVars : [
		'instVarName1',
		'instVarName2'
	],
	#classVars : [
		'Shared1',
		'Shared2',
		'SharedOnlyInClassSideSubclass'
	],
	#category : 'Refactoring-DataForTesting-StaticModel',
	#package : 'Refactoring-DataForTesting',
	#tag : 'StaticModel'
}

{ #category : 'accessing' }
MyClassARoot class >> accessingFromClassSide [

	^ Shared1
]

{ #category : 'accessing' }
MyClassARoot >> accessing [

	^ instVarName1 
]

{ #category : 'accessing' }
MyClassARoot >> accessingSharedVariable [

	^ Shared1 
]

{ #category : 'initialization' }
MyClassARoot >> initialize [

	<ignoreUnusedClassVariables: #( ClassVarName2 ClassVarName1 )>
	<ignoreUnusedVariables: #( instVarName1 instVarName2 )>
	super initialize
]
