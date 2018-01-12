Class {
	#name : #TaRemoveMethod,
	#superclass : #TaSingleComposition,
	#instVars : [
		'removedSelectors'
	],
	#category : #'TraitsV2-Compositions'
}

{ #category : #'instance creation' }
TaRemoveMethod class >> remove: anArrayOfSelectors to: aTalent [
	^ self new
		removedSelectors: anArrayOfSelectors;
		inner: aTalent;
		yourself
]

{ #category : #accessing }
TaRemoveMethod >> compiledMethodAt: aSelector [
	(removedSelectors includes: aSelector ) ifTrue:[NotFound signalFor: aSelector].
	^ inner compiledMethodAt: aSelector.
]

{ #category : #copying }
TaRemoveMethod >> copyTraitExpression [
	^ self class remove: removedSelectors to: inner
]

{ #category : #accessing }
TaRemoveMethod >> methods [
	^ inner methods reject: [ :e | removedSelectors includes: e selector ]
]

{ #category : #accessing }
TaRemoveMethod >> removedSelectors [
	^ removedSelectors
]

{ #category : #accessing }
TaRemoveMethod >> removedSelectors: anObject [
	removedSelectors := anObject
]

{ #category : #accessing }
TaRemoveMethod >> selectors [
	^ inner selectors reject: [ :e | removedSelectors includes: e ]
]

{ #category : #printing }
TaRemoveMethod >> traitCompositionExpression [
	
	^ String streamContents: [ :s |
	  s 
		nextPutAll: self inner traitCompositionExpressionWithParens ;
		nextPutAll: ' - '.
		removedSelectors printAsSelfEvaluatingFormOn: s.
	]
]

{ #category : #accessing }
TaRemoveMethod >> traitDefining: aSelector [
	(removedSelectors includes: aSelector ) ifTrue:[NotFound signalFor: aSelector].
	^ inner traitDefining: aSelector
]
