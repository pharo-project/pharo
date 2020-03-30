Class {
	#name : #CoSession,
	#superclass : #Object,
	#instVars : [
		'vocabularyCache'
	],
	#category : #'HeuristicCompletion-Model-SessionHeuristic'
}

{ #category : #vocabulary }
CoSession >> addVocabulary: aString at: aDateAndTime [ 
	
	vocabularyCache at: aString put: aDateAndTime
]

{ #category : #initialization }
CoSession >> initialize [

	super initialize.
	vocabularyCache := LRUCache new maximumWeight: 1000
]

{ #category : #accessing }
CoSession >> size [
	^ vocabularyCache size
]

{ #category : #accessing }
CoSession >> words [
	
	^ vocabularyCache keys
]
