"
I give meta gestures their meaning: a special gesture (cmd-click on macOS, alt-click elsewhere) opens a halo on the morph under the hand, the meta menu gesture opens its meta menu, and escape with a modifier opens the morph menu of the keyboard focus holder. I also keep the halo up to date from the events I see: a click elsewhere or a drop removes it, arrow keys move its morph.

Add me to a world: aWorld addRootEventHandler: HaloRootEventHandler new.
"
Class {
	#name : 'HaloRootEventHandler',
	#superclass : 'MorphRootEventHandler',
	#category : 'Morphic-Halos',
	#package : 'Morphic-Halos'
}

{ #category : 'event handling' }
HaloRootEventHandler >> handleEvent: anEvent in: aWorld [
	"Open a meta menu (shift + special gesture) or a halo (special gesture: cmd-click on macOS, alt-click elsewhere) on the morph under the hand.
	Keep the halo up to date: clicks elsewhere and drops remove it, arrow keys move its morph.
	Escape with a modifier opens the morph menu of the keyboard focus holder."

	anEvent isKeyboard ifTrue: [
		anEvent isKeyDown ifTrue: [ ^ self openMorphMenu: anEvent in: aWorld ].
		anEvent isKeystroke ifTrue: [ ^ self moveHaloMorph: anEvent ].
		^ false ].
	anEvent isDropEvent ifTrue: [
		anEvent hand removeHalo.
		^ false ].
	(anEvent isMouse and: [ anEvent isMouseDown ]) ifFalse: [ ^ false ].
	Morph cmdGesturesEnabled ifTrue: [
		anEvent isSpecialMetaMenuGesture ifTrue: [ ^ self openMetaMenu: anEvent in: aWorld ].
		(anEvent isSpecialGesture and: [ self openHalo: anEvent in: aWorld ]) ifTrue: [ ^ true ] ].
	^ self removeHaloFromClick: anEvent in: aWorld
]

{ #category : 'private' }
HaloRootEventHandler >> morphsUnder: anEvent in: aWorld [
	"Answer the unlocked morphs of aWorld under anEvent, innermost first. Morphs of a world embedded in aWorld belong to that world"

	^ (aWorld morphsAt: anEvent position unlocked: true) select: [ :each | each ~~ aWorld and: [ each world == aWorld ] ]
]

{ #category : 'private' }
HaloRootEventHandler >> moveHaloMorph: aKeyboardEvent [
	"Arrow keys move or resize the morph that has the halo, when nothing has the keyboard focus. Let the key go on"

	aKeyboardEvent hand keyboardFocus ifNil: [
		aKeyboardEvent hand selectedObject ifNotNil: [ :selected | selected moveOrResizeFromKeystroke: aKeyboardEvent ] ].
	^ false
]

{ #category : 'private' }
HaloRootEventHandler >> openHalo: mouseDownEvent in: aWorld [
	"Allow a mouse-sensitive morph to be moved, or bring up its halo.
	A special gesture inside the hand's halo transfers it. Morphs that handle the blue button themselves keep the gesture."

	| under aMorph h doNotDrag |
	mouseDownEvent hand halo ifNotNil: [ :halo |
		(halo containsPoint: mouseDownEvent position event: mouseDownEvent) ifTrue: [ "The halo gets the click that transfers it"
			mouseDownEvent hand newMouseFocus: halo event: mouseDownEvent.
			halo handleSpecialGesture: mouseDownEvent.
			^ true ] ].
	under := self morphsUnder: mouseDownEvent in: aWorld.
	(under anySatisfy: [ :each | (each handlerForBlueButtonDown: mouseDownEvent) isNotNil ]) ifTrue: [ ^ false ].
	aMorph := under detect: [ :each | each wantsHaloFromClick ] ifNone: [ ^ false ].
	h := mouseDownEvent hand halo.
	"Prevent wrap around halo transfers originating from throwing the event back in"
	doNotDrag := false.
	h ifNotNil: [
		h innerTarget == aMorph ifTrue: [ doNotDrag := true ].
		(h innerTarget hasOwner: aMorph) ifTrue: [ doNotDrag := true ].
		(aMorph hasOwner: h target) ifTrue: [ doNotDrag := true ] ].

	h := aMorph addHalo: mouseDownEvent.
	h ifNil: [ ^ true ].
	doNotDrag ifTrue: [ ^ true ].
	"Initiate drag transition if requested"
	mouseDownEvent hand
		waitForClicksOrDrag: h
		event: mouseDownEvent
		selectors: { nil. nil. nil. #dragTarget: }
		threshold: 5.
	"Pass focus explicitly here"
	mouseDownEvent hand newMouseFocus: h.
	^ true
]

{ #category : 'private' }
HaloRootEventHandler >> openMetaMenu: anEvent in: aWorld [

	| target menu |
	target := (self morphsUnder: anEvent in: aWorld) detect: [ :each | each wantsMetaMenu ] ifNone: [ ^ false ].
	menu := target buildMetaMenu: anEvent.
	target addTitleForHaloMenu: menu.
	menu popUpEvent: anEvent in: aWorld.
	^ true
]

{ #category : 'private' }
HaloRootEventHandler >> openMorphMenu: aKeyboardEvent in: aWorld [

	(aKeyboardEvent keyCharacter = Character escape and: [ aKeyboardEvent anyModifierKeyPressed ]) ifFalse: [ ^ false ].
	(aKeyboardEvent hand keyboardFocus ifNil: [ aWorld ]) yellowButtonActivity: false.
	"The key press that follows the key down would still reach the focus holder"
	aKeyboardEvent supressNextKeyPress: true.
	^ true
]

{ #category : 'private' }
HaloRootEventHandler >> removeHaloFromClick: anEvent in: aWorld [
	"A click on the world shakes off the halo; a click elsewhere removes it unless it stays up. Let the click go on"

	| under |
	under := self morphsUnder: anEvent in: aWorld.
	under ifEmpty: [ anEvent hand removeHalo ] ifNotEmpty: [ anEvent hand removeHaloFromClick: anEvent on: under first ].
	^ false
]
