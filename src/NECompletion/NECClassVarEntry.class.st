"
I represent a class Variable
"
Class {
	#name : #NECClassVarEntry,
	#superclass : #NECEntry,
	#category : #'NECompletion-Model'
}

{ #category : #operations }
NECClassVarEntry >> guessTypeWith: anECContext [ 
	^ anECContext guessClassVarClass: contents
]

{ #category : #accessing }
NECClassVarEntry >> label [
	^ 'class variable'
]

{ #category : #accessing }
NECClassVarEntry >> type [
	^ #classVar
]
