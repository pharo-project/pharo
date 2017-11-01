"
I represent super.
"
Class {
	#name : #NECSuperEntry,
	#superclass : #NECEntry,
	#category : 'NECompletion-Model'
}

{ #category : #operations }
NECSuperEntry >> guessTypeWith: anECContext [ 
	^ anECContext theClass ifNotNil: [anECContext theClass superclass]
]

{ #category : #accessing }
NECSuperEntry >> label [
	^ 'super'
]
