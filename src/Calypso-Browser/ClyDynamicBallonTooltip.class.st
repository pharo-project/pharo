"
I am a hook to get dynamically updatable balloon tooltip.

To install me into the morph use following expression:

	ClyDynamicBallonTooltip from: ['test string'] installInto: aMorph
"
Class {
	#name : #ClyDynamicBallonTooltip,
	#superclass : #Object,
	#instVars : [
		'block'
	],
	#category : #'Calypso-Browser-UI'
}

{ #category : #'instance creation' }
ClyDynamicBallonTooltip class >> from: aBlock [
	^self new 
		block: aBlock
]

{ #category : #'instance creation' }
ClyDynamicBallonTooltip class >> from: aBlock installInto: aMorph [
	^(self from: aBlock)
		installInto: aMorph; 
		yourself
]

{ #category : #converting }
ClyDynamicBallonTooltip >> asString [
	^block value
]

{ #category : #accessing }
ClyDynamicBallonTooltip >> block [
	^ block
]

{ #category : #accessing }
ClyDynamicBallonTooltip >> block: anObject [
	block := anObject
]

{ #category : #installation }
ClyDynamicBallonTooltip >> installInto: aMorph [
	"Dynamic update is done by #asString converting.
	But simple aMorph setBalloonText: also converts given argument to string.
	So here I use low level morph extension logic which simply assigns given tooltip instance"
	aMorph assureExtension balloonText: self
]
