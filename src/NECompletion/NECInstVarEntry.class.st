"
I represent an instance variable.
"
Class {
	#name : #NECInstVarEntry,
	#superclass : #NECCVariableEntry,
	#category : #'NECompletion-Model'
}

{ #category : #accessing }
NECInstVarEntry >> hightlightSymbol [
	^ #instVar
]

{ #category : #accessing }
NECInstVarEntry >> label [
	^ 'instance variable'
]
