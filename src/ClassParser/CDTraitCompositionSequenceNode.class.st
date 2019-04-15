"
use: T1 + T2
"
Class {
	#name : #CDTraitCompositionSequenceNode,
	#superclass : #CDTraitCompositionNode,
	#instVars : [
		'sequence'
	],
	#category : #'ClassParser-Model'
}

{ #category : #testing }
CDTraitCompositionSequenceNode >> isTraitSequenceNode [
	
	^ true
]

{ #category : #accessing }
CDTraitCompositionSequenceNode >> sequence [
	^sequence
]

{ #category : #accessing }
CDTraitCompositionSequenceNode >> sequence: aCollection [ 
	sequence := aCollection
]

{ #category : #'as yet unclassified' }
CDTraitCompositionSequenceNode >> toString [ 
	|res|
	res:=(self sequence at: 1) toString,
		' + ',
		(self sequence at: 2) toString.
	^res	
	
]
