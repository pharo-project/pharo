"
I represent an instance variable.
"
Class {
	#name : 'CoInstVarEntry',
	#superclass : 'CoVariableEntry',
	#category : 'NECompletion-Model',
	#package : 'NECompletion',
	#tag : 'Model'
}

{ #category : 'accessing' }
CoInstVarEntry >> hightlightSymbol [
	^ #instVar
]

{ #category : 'accessing' }
CoInstVarEntry >> rawLabel [
	^ 'instance variable'
]
