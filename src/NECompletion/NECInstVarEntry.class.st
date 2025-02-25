"
I represent an instance variable.
"
Class {
	#name : 'NECInstVarEntry',
	#superclass : 'NECVariableEntry',
	#category : 'NECompletion-Model',
	#package : 'NECompletion',
	#tag : 'Model'
}

{ #category : 'accessing' }
NECInstVarEntry >> hightlightSymbol [
	^ #instVar
]

{ #category : 'accessing' }
NECInstVarEntry >> label [
	^ 'instance variable'
]
