"
I am test object used for the WeakRegisty tests.
Upon finalization I trigger a custom finalizationAction (usually a Block)
"
Class {
	#name : #MockFinalizerAction,
	#superclass : #Object,
	#instVars : [
		'finalizationAction'
	],
	#category : #'Collections-Tests-Weak'
}

{ #category : #finalization }
MockFinalizerAction >> finalizationAction: aValuable [
	finalizationAction := aValuable
]

{ #category : #finalization }
MockFinalizerAction >> finalize [
	finalizationAction value
]

{ #category : #finalization }
MockFinalizerAction >> finalizeValues [
	finalizationAction value
]
