"
I give meta gestures their meaning in the Pharo desktop: a special gesture (cmd-click on macOS, alt-click elsewhere) opens a halo on the morph, the meta menu gesture opens its meta menu, and the yellow button opens its morph menu.

Add me to a root morph, usually the world: aWorld addRootEventHandler: HaloRootEventHandler new.
"
Class {
	#name : 'HaloRootEventHandler',
	#superclass : 'MorphRootEventHandler',
	#category : 'Morphic-Halos',
	#package : 'Morphic-Halos'
}

{ #category : 'event handling' }
HaloRootEventHandler >> handleMetaMenuGesture: evt for: aMorph [

	| menu |
	aMorph world ifNil: [ ^ false ].

	menu := aMorph buildMetaMenu: evt.
	aMorph addTitleForHaloMenu: menu.
	menu popUpEvent: evt in: aMorph world.
	^ true
]

{ #category : 'event handling' }
HaloRootEventHandler >> handleSpecialGesture: mouseDownEvent for: aMorph [
	"Special gestures (cmd-mouse on the Macintosh; Alt-mouse on Windows and Unix) allow a mouse-sensitive morph to be moved or bring up a halo for the morph."

	| h tfm doNotDrag |
	h := mouseDownEvent hand halo.
	"Prevent wrap around halo transfers originating from throwing the event back in"
	doNotDrag := false.
	h ifNotNil: [
		h innerTarget == aMorph ifTrue: [ doNotDrag := true ].
		(h innerTarget hasOwner: aMorph) ifTrue: [ doNotDrag := true ].
		(aMorph hasOwner: h target) ifTrue: [ doNotDrag := true ] ].

	tfm := (aMorph transformedFrom: nil) inverseTransformation.

	"cmd-drag on flexed morphs works better this way"
	h := aMorph addHalo: (mouseDownEvent transformedBy: tfm).
	h ifNil: [ ^ true ].
	doNotDrag ifTrue: [ ^ true ].
	"Initiate drag transition if requested"
	mouseDownEvent hand
		waitForClicksOrDrag: h
		event: (mouseDownEvent transformedBy: tfm)
		selectors: { nil. nil. nil. #dragTarget: }
		threshold: 5.
	"Pass focus explicitly here"
	mouseDownEvent hand newMouseFocus: h.
	^ true
]

{ #category : 'event handling' }
HaloRootEventHandler >> handleYellowButtonMenu: shiftState for: aMorph [

	| menu |
	menu := aMorph buildYellowButtonMenu: aMorph activeHand.
	menu addTitle: aMorph externalName icon: (aMorph iconOrThumbnailOfSize: 28).
	menu popUpInWorld: aMorph currentWorld.
	^ true
]

{ #category : 'notifications' }
HaloRootEventHandler >> morph: aMorph wrappedInto: aWrapperMorph [
	"The halo follows the morph into its wrapper"

	aMorph halo ifNotNil: [ :halo | halo setTarget: aWrapperMorph ]
]

{ #category : 'notifications' }
HaloRootEventHandler >> morphDeleted: aMorph [

	aMorph removeHalo
]

{ #category : 'notifications' }
HaloRootEventHandler >> morphDropped: aMorph into: aTargetMorph event: anEvent [
	"A morph dropped into another owner loses its halo. A dropped morph containing the halo's target (e.g. a menu grabbed by one of its items) gets the halo"

	| halo |
	(aMorph formerOwner isNotNil and: [ aMorph formerOwner ~~ aTargetMorph ])
		ifTrue: [ aMorph removeHalo ].
	halo := anEvent hand halo.
	(halo isNotNil and: [ halo target hasOwner: aMorph ])
		ifTrue: [ aMorph addHalo: anEvent ]
]

{ #category : 'notifications' }
HaloRootEventHandler >> morphKeyStroke: anEvent for: aMorph [
	"Arrow keys move or resize the morph that has the halo"

	aMorph selectedObject ifNotNil: [ :selected | selected moveOrResizeFromKeystroke: anEvent ]
]

{ #category : 'notifications' }
HaloRootEventHandler >> morphMouseDown: anEvent for: aMorph [
	"A click on the world shakes off the halo; a click elsewhere removes it unless it stays up"

	aMorph isWorldMorph ifTrue: [ ^ anEvent hand removeHalo ].
	anEvent hand removeHaloFromClick: anEvent on: aMorph
]
