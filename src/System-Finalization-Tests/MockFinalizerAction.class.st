"
I am test object used for the WeakRegisty tests.
Upon finalization I trigger a custom finalizationAction (usually a Block)
"
Class {
	#name : 'MockFinalizerAction',
	#superclass : 'Object',
	#instVars : [
		'finalizationAction'
	],
	#category : 'System-Finalization-Tests',
	#package : 'System-Finalization-Tests'
}

{ #category : 'finalization' }
MockFinalizerAction >> finalizationAction: aValuable [
	finalizationAction := aValuable
]

{ #category : 'finalization' }
MockFinalizerAction >> finalize [
	finalizationAction value
]
