"
I represent a class Variable
"
Class {
	#name : 'CoClassVarEntry',
	#superclass : 'CoVariableEntry',
	#category : 'NECompletion-Model',
	#package : 'NECompletion',
	#tag : 'Model'
}

{ #category : 'accessing' }
CoClassVarEntry >> hightlightSymbol [
	^ #classVar
]

{ #category : 'accessing' }
CoClassVarEntry >> rawLabel [
	^ 'class variable'
]
