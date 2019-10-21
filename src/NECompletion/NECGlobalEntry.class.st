"
I represent a global variable.
"
Class {
	#name : #NECGlobalEntry,
	#superclass : #NECEntry,
	#category : #'NECompletion-Model'
}

{ #category : #accessing }
NECGlobalEntry >> label [
	^ 'global'
]

{ #category : #accessing }
NECGlobalEntry >> type [
	^ #globalVar
]
