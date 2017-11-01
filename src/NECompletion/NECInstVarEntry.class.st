"
I represent an instance variable.
"
Class {
	#name : #NECInstVarEntry,
	#superclass : #NECEntry,
	#category : 'NECompletion-Model'
}

{ #category : #operations }
NECInstVarEntry >> guessTypeWith: anECContext [ 
	^ anECContext guessInstVarClass: contents
]

{ #category : #testing }
NECInstVarEntry >> isInstance [
	^true
]

{ #category : #accessing }
NECInstVarEntry >> label [
	^ 'instance variable'
]

{ #category : #accessing }
NECInstVarEntry >> type [
	^ #instVar
]
