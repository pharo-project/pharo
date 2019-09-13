"
I am a critique that will propose to the user to remove a method.
"
Class {
	#name : #ReRemoveMethodCritique,
	#superclass : #ReAbstractCritique,
	#instVars : [
		'selector'
	],
	#category : #'Renraku-Critiques'
}

{ #category : #'instance creation' }
ReRemoveMethodCritique class >> for: anEntity selector: aSelector by: aRule [
	^ (self for: anEntity by: aRule)
		tinyHint: aSelector;
		yourself
]

{ #category : #accessing }
ReRemoveMethodCritique >> change [
	^ RBRemoveMethodChange remove: self tinyHint from: self entity
]

{ #category : #testing }
ReRemoveMethodCritique >> providesChange [
	^ true
]
