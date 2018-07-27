"
I represent a local variable: either a temporary variables, a block temporary or a method arguments. SHOUT ranges objects are used to identify tempvariables and others. 

In addition tools such as the workspace may turn their bindings into instance of me so that the completion algo can take them into account. 
"
Class {
	#name : #NECLocalEntry,
	#superclass : #NECEntry,
	#category : #'NECompletion-Model'
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
