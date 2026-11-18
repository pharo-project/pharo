"
I give meta gestures their meaning in the Pharo desktop: a special gesture (cmd-click on macOS, alt-click elsewhere) opens a halo on the morph, and the meta menu gesture opens its meta menu.

Set me on a root morph, usually the world: aWorld rootEventHandler: HaloRootEventHandler new.
"
Class {
	#name : 'HaloRootEventHandler',
	#superclass : 'MorphRootEventHandler',
	#category : 'Morphic-Base-Halos',
	#package : 'Morphic-Base',
	#tag : 'Halos'
}

{ #category : 'event handling' }
HaloRootEventHandler >> handleMetaMenuGesture: evt for: aMorph [

	| menu |
	aMorph world ifNil: [ ^ self ].

	menu := aMorph buildMetaMenu: evt.
	aMorph addTitleForHaloMenu: menu.
	menu popUpEvent: evt in: aMorph world
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
	h ifNil: [ ^ self ].
	doNotDrag ifTrue: [ ^ self ].
	"Initiate drag transition if requested"
	mouseDownEvent hand
		waitForClicksOrDrag: h
		event: (mouseDownEvent transformedBy: tfm)
		selectors: { nil. nil. nil. #dragTarget: }
		threshold: 5.
	"Pass focus explicitly here"
	mouseDownEvent hand newMouseFocus: h
]
