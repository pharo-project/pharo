"
I represent a global variable.
"
Class {
	#name : #NECGlobalEntry,
	#superclass : #NECCVariableEntry,
	#category : #'NECompletion-Model'
}

{ #category : #accessing }
NECGlobalEntry >> hightlightSymbol [
	^ #globalVar
]

{ #category : #accessing }
NECGlobalEntry >> label [
	^ 'global'
]
