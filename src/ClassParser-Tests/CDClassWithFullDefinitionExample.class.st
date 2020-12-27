Class {
	#name : #CDClassWithFullDefinitionExample,
	#superclass : #Object,
	#instVars : [
		'instVar1',
		'instVar2'
	],
	#classVars : [
		'ClassVar1',
		'ClassVar2'
	],
	#classInstVars : [
		'classSideVar1',
		'classSideVar2'
	],
	#category : #'ClassParser-Tests'
}

{ #category : #accessing }
CDClassWithFullDefinitionExample class >> accessingVariables [ 
	classSideVar1 := classSideVar2 := 33
]

{ #category : #accessing }
CDClassWithFullDefinitionExample >> accessingVariables [

	instVar1 := instVar2
]
