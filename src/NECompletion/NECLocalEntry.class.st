"
I represent a local variable
"
Class {
	#name : #NECLocalEntry,
	#superclass : #NECEntry,
	#category : 'NECompletion-Model'
}

{ #category : #operations }
NECLocalEntry >> guessTypeWith: anECContext [ 
	^ (anECContext guessTempVarClass: contents type: self type) 
		ifNil: [anECContext guessArgument: contents]
]

{ #category : #testing }
NECLocalEntry >> isLocal [
	^true
]

{ #category : #accessing }
NECLocalEntry >> label [
	^ 'local variable'
]
