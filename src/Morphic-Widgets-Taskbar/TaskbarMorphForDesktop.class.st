Class {
	#name : #TaskbarMorphForDesktop,
	#superclass : #TaskbarMorph,
	#instVars : [
		'desktop'
	],
	#category : #'Morphic-Widgets-Taskbar'
}

{ #category : #'instance creation' }
TaskbarMorphForDesktop class >> newWithDesktop: aMorph [

	| taskbar |
	taskbar := self new.
	taskbar desktop:aMorph.
	^ taskbar
]

{ #category : #accessing }
TaskbarMorphForDesktop >> desktop [

	^ desktop
]

{ #category : #taskbar }
TaskbarMorphForDesktop >> desktop: aMorph [

	desktop := aMorph
]

{ #category : #taskbar }
TaskbarMorphForDesktop >> worldMorphs [
	"Answer the world's submorphs plus those in hand.
	Nasty case since hand removes the morph before dropping"
	^self desktop submorphs, 
		((self tasks
			select: [:t | t morph owner = self world activeHand])
				collect: [:t | t morph])
]
