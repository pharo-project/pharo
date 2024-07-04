"
This is a stupid comment for a stupid class.
"
Class {
	#name : 'MyClassA',
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

{ #category : 'tests - test data' }
MyClassA >> methodForPushDown [
	"My temporary comment"
	^ 42
]

{ #category : 'dummy methods' }
MyClassA >> stupidMethodForStupidRule [

	instVarName1 := instVarName2 := ClassVarName1 := ClassVarName2 := 0
]
