"
I represent a refactoring performed in the system. 
"
Class {
	#name : #EpRefactoring,
	#superclass : #EpEvent,
	#category : 'Epicea-Model'
}

{ #category : #visitor }
EpRefactoring >> accept: aChangeVisitor [ 
	^ aChangeVisitor visitRefactoring: self
]

{ #category : #converting }
EpRefactoring >> asRBRefactoring [
	"Answer the RBRefactoring corresponding to myself."
	
	^ self subclassResponsibility 
]

{ #category : #testing }
EpRefactoring >> canBuildRBRefactoring [
	^ true
]

{ #category : #testing }
EpRefactoring >> isEpRefactoring [

	^ true
]
