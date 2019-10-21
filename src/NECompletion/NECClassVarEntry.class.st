"
I represent a class Variable
"
Class {
	#name : #NECClassVarEntry,
	#superclass : #NECEntry,
	#category : #'NECompletion-Model'
}

{ #category : #accessing }
NECClassVarEntry >> label [
	^ 'class variable'
]

{ #category : #accessing }
NECClassVarEntry >> type [
	^ #classVar
]
