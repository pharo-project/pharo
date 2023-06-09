"
I am a notice for syntaxic or semantic errors
"
Class {
	#name : #RBErrorNotice,
	#superclass : #RBNotice,
	#category : #'AST-Core-Notice'
}

{ #category : #testing }
RBErrorNotice >> isError [

	^ true
]
