Class {
	#name : #CoMockFetcherToRecordResets,
	#superclass : #CoFetcher,
	#instVars : [
		'resetingCount'
	],
	#category : #'HeuristicCompletion-Tests-Core'
}

{ #category : #enumerating }
CoMockFetcherToRecordResets >> entriesDo: aBlock [

	1 to: 1000 do: [ :anIndex | (NECGlobalEntry contents: anIndex printString node: nil) ]
]

{ #category : #initialization }
CoMockFetcherToRecordResets >> initialize [ 

	super initialize.
	resetingCount := 0.
]

{ #category : #initialization }
CoMockFetcherToRecordResets >> reset [ 

	resetingCount := resetingCount + 1.
	super reset
]

{ #category : #accessing }
CoMockFetcherToRecordResets >> resetingCount [
	
	^ resetingCount 
]
