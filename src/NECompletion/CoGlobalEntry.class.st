"
I represent a global variable.
"
Class {
	#name : 'CoGlobalEntry',
	#superclass : 'CoVariableEntry',
	#category : 'NECompletion-Model',
	#package : 'NECompletion',
	#tag : 'Model'
}

{ #category : 'accessing' }
CoGlobalEntry >> hightlightSymbol [
	^ #globalVar
]

{ #category : 'accessing' }
CoGlobalEntry >> rawLabel [
	^ 'global'
]
