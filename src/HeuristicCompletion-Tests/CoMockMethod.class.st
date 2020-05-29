Class {
	#name : #CoMockMethod,
	#superclass : #Object,
	#instVars : [
		'ast',
		'methodClass'
	],
	#category : #'HeuristicCompletion-Tests-Core'
}

{ #category : #accessing }
CoMockMethod >> ast [
	^ ast
]

{ #category : #accessing }
CoMockMethod >> methodClass [
	
	^ methodClass
]

{ #category : #accessing }
CoMockMethod >> methodClass: aCoMockClass [ 
	methodClass := aCoMockClass
]

{ #category : #accessing }
CoMockMethod >> source: aString [ 
	
	ast := RBParser parseMethod: aString 
]
