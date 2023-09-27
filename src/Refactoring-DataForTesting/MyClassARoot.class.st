Class {
	#name : 'MyClassARoot',
	#superclass : 'Object',
	#instVars : [
		'instVarName1',
		'instVarName2'
	],
	#classVars : [
		'ClassVarName1',
		'ClassVarName2'
	],
	#category : 'Refactoring-DataForTesting-StaticModel',
	#package : 'Refactoring-DataForTesting',
	#tag : 'StaticModel'
}

{ #category : 'accessing' }
MyClassARoot >> accessing [

	^ instVarName1 
]
