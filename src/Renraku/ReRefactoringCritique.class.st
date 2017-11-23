"
A critique which has a refactoring needed to solve the issue
"
Class {
	#name : #ReRefactoringCritique,
	#superclass : #ReAbstractCritique,
	#instVars : [
		'refactoring',
		'alreadyExecuted'
	],
	#category : #'Renraku-Critiques'
}

{ #category : #accessing }
ReRefactoringCritique >> change [
	"return changes provided by the refactoring"
	
	alreadyExecuted ifFalse: [
		refactoring primitiveExecute.
		alreadyExecuted := true ].
	
	^ refactoring changes
]

{ #category : #initialization }
ReRefactoringCritique >> initialize [
	
	alreadyExecuted := false
]

{ #category : #testing }
ReRefactoringCritique >> providesChange [
	
	^ true
]

{ #category : #accessing }
ReRefactoringCritique >> refactoring [
	^ refactoring
]

{ #category : #accessing }
ReRefactoringCritique >> refactoring: anObject [
	refactoring := anObject
]
