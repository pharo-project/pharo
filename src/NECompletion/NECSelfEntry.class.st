"
I represent self
"
Class {
	#name : #NECSelfEntry,
	#superclass : #NECEntry,
	#category : #'NECompletion-Model'
}

{ #category : #operations }
NECSelfEntry >> guessTypeWith: anECContext [ 
	^ anECContext theClass
]

{ #category : #accessing }
NECSelfEntry >> label [
	^ 'self'
]
