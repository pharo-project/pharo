"
I represent spotter search candidate which performs goto operation.
Actual action is defined in actionBlock by ClySpotterModel. 
 
Internal Representation and Key Implementation Points.

    Instance Variables
	actionBlock:		<BlockClosure>
	icon:		<Form>
	name:		<String>
"
Class {
	#name : #ClyGoToSpotterCandidate,
	#superclass : #Object,
	#instVars : [
		'targetView',
		'name',
		'icon',
		'actionBlock'
	],
	#category : #'Calypso-SystemPlugins-Spotter'
}

{ #category : #'instance creation' }
ClyGoToSpotterCandidate class >> target: aView named: aString icon: aForm using: aBlock [
	^(self target: aView named: aString using: aBlock)
		icon: aForm 
]

{ #category : #'instance creation' }
ClyGoToSpotterCandidate class >> target: aView named: aString using: aBlock [
	^self new 
		targetView: aView;
		name: aString;
		actionBlock: aBlock
]

{ #category : #accessing }
ClyGoToSpotterCandidate >> actionBlock [
	^ actionBlock
]

{ #category : #accessing }
ClyGoToSpotterCandidate >> actionBlock: anObject [
	actionBlock := anObject
]

{ #category : #execution }
ClyGoToSpotterCandidate >> activate [
	actionBlock value
]

{ #category : #accessing }
ClyGoToSpotterCandidate >> icon [
	^ icon
]

{ #category : #accessing }
ClyGoToSpotterCandidate >> icon: anObject [
	icon := anObject
]

{ #category : #accessing }
ClyGoToSpotterCandidate >> name [
	^ name
]

{ #category : #accessing }
ClyGoToSpotterCandidate >> name: anObject [
	name := anObject
]

{ #category : #spotter }
ClyGoToSpotterCandidate >> spotterPreviewCodeIn: aComposite [
	<spotterPreview: 10>
	
	^targetView spotterPreviewMorphIn: aComposite
]

{ #category : #accessing }
ClyGoToSpotterCandidate >> targetView [
	^ targetView
]

{ #category : #accessing }
ClyGoToSpotterCandidate >> targetView: anObject [
	targetView := anObject
]
