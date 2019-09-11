"
I am the reification of the Keymap target.  I am the one that receives the actions.
"
Class {
	#name : #KMTarget,
	#superclass : #Object,
	#instVars : [
		'morph',
		'targetSelector'
	],
	#category : #'Keymapping-Core-Base'
}

{ #category : #'instance creation' }
KMTarget class >> for: aTarget in: aMorph [
	^ (self new)
		targetSelector: aTarget;
		morph: aMorph;
		yourself
]

{ #category : #protocol }
KMTarget >> announcer [

	^ self morph announcer
]

{ #category : #accessing }
KMTarget >> dispatch: buffer [

	morph kmDispatcher dispatch: buffer
]

{ #category : #accessing }
KMTarget >> morph [
	^morph
]

{ #category : #accessing }
KMTarget >> morph: aMorph [
	morph := aMorph
]

{ #category : #iterating }
KMTarget >> nextForKmChain: aKMDispatchChain [ 
	^self ownerTarget
]

{ #category : #accessing }
KMTarget >> ownerTarget [
	morph owner ifNil: [ ^nil ].
	^morph owner kmDispatcher target.
]

{ #category : #accessing }
KMTarget >> realTarget [
	^morph perform: self targetSelector
]

{ #category : #accessing }
KMTarget >> targetSelector [
	^targetSelector ifNil: [ #yourself ]
]

{ #category : #accessing }
KMTarget >> targetSelector: aTargetSelector [
	targetSelector := aTargetSelector
]
