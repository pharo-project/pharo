"
I represent a litlle world for an other window open out of the pharo window

I create a new world and i know my render, my state and my root.

    Instance Variables
	root:		MorphicRoot
	state:		WorldState
	submorph:		morphe
	worldRendered:		worldrendered


    Implementation Points
"
Class {
	#name : #MiniWorld,
	#superclass : #Morph,
	#instVars : [
		'backgroundMorph',
		'manager',
		'worldState'
	],
	#category : #'Morphic-Core'
}

{ #category : #'initialize-release' }
MiniWorld class >> doOneCycle [

	WorldState doDrawCycleWith: [
		| extraWorldsToDraw |
		extraWorldsToDraw := WorldMorph extraWorldListMutex critical: [
			                     WorldMorph extraWorldList ].
		extraWorldsToDraw do: [ :world | world doOneCycle ].

		(self currentWorld isNotNil and: [
			 (extraWorldsToDraw includes: self currentWorld) not ]) ifTrue: [
			self currentWorld doOneCycle ] ]
]

{ #category : #'world state' }
MiniWorld >> activateCursor: aCursor withMask: maskForm [

	worldState worldRenderer activateCursor: aCursor withMask: maskForm
]

{ #category : #structure }
MiniWorld >> activeHand [

	^ worldState activeHand
]

{ #category : #structure }
MiniWorld >> activeHand: aHandMorph [
	"Temporarily retained for old main event loops"

	worldState activeHand: aHandMorph
]

{ #category : #stepping }
MiniWorld >> addAlarm: aSelector withArguments: argArray for: aTarget at: scheduledTime [
	"Add a new alarm with the given set of parameters"

	worldState
		addAlarm: aSelector
		withArguments: argArray
		for: aTarget
		at: scheduledTime
]

{ #category : #'API - opening' }
MiniWorld >> addHand: aHandMorph [
	"Add the given hand to the list of hands for this world."

	aHandMorph owner ifNotNil: [ aHandMorph owner removeHand: aHandMorph ].
	worldState addHand: aHandMorph.
	aHandMorph privateOwner: self.
	^ aHandMorph
]

{ #category : #'submorphs - add/remove' }
MiniWorld >> addMorph: aMorph [

	| desktop |
	desktop := submorphs
		           detect: [ :d | d isDesktop ]
		           ifFound: [ :d | ^ d addMorphInFrontOfLayer: aMorph ]
		           ifNone: [ self ].
	"I will be the owner (and I will act as a world)"
	aMorph privateOwner: desktop.

	"check for the need of steppiong stepping"
	aMorph wantsSteps ifTrue: [ self startStepping: aMorph ].

	desktop submorphs asOrderedCollection addLast: aMorph
]

{ #category : #'world state' }
MiniWorld >> addMorphFront: aMorph [

	^ self addMorphInFrontOfLayer: aMorph
]

{ #category : #'submorphs - add/remove' }
MiniWorld >> addMorphInFrontOfLayer: aMorph [

	| desktop |
	desktop := submorphs
		           detect: [ :d | d isDesktop ]
		           ifFound: [ :d | ^ d addMorphInFrontOfLayer: aMorph ]
		           ifNone: [ self ].
	"I will be the owner (and I will act as a world)"
	aMorph privateOwner: desktop.

	"check for the need of steppiong stepping"
	aMorph wantsSteps ifTrue: [ self startStepping: aMorph ].

	desktop submorphs asOrderedCollection addFirst: aMorph
]

{ #category : #accessing }
MiniWorld >> addSubmorphs: aMorph [

	submorphs addFirst: aMorph
]

{ #category : #background }
MiniWorld >> backgroundMorph [
	"Answer the background morph if any."

	^ backgroundMorph
]

{ #category : #background }
MiniWorld >> backgroundMorph: aMorph [
	"Set the background morph.
	Probably best if locked prior to adding."

	self backgroundMorph ifNotNil: [ self backgroundMorph delete ].
	backgroundMorph := aMorph.
	aMorph ifNotNil: [
		aMorph bounds: self bounds.
		self addMorphBack: aMorph ]
]

{ #category : #accessing }
MiniWorld >> basicLabel [

	^ nil
]

{ #category : #cursor }
MiniWorld >> beCursorOwner [
	"nil actually"

	
]

{ #category : #initialization }
MiniWorld >> becomeActiveDuring: aBlock [

	aBlock value
]

{ #category : #'world state' }
MiniWorld >> currentCursor [

	^ worldState currentCursor
]

{ #category : #accessing }
MiniWorld >> currentCursor: aCursor [

	^ worldState currentCursor: aCursor
]

{ #category : #accessing }
MiniWorld >> currentWindow [

	^ self
]

{ #category : #'submorphs - add/remove' }
MiniWorld >> delete [
	"self worldState worldRenderer deactivate."

	WorldMorph removeExtraWorld: self
]

{ #category : #'balloon help' }
MiniWorld >> deleteBalloonTarget: aMorph [
	"Delete the balloon help targeting the given morph"

	self handsDo: [ :h | h deleteBalloonTarget: aMorph ]
]

{ #category : #'submorphs - add/remove' }
MiniWorld >> deleteByCross [

	self delete
]

{ #category : #display }
MiniWorld >> displayArea [

	^ self worldState worldRenderer usableArea
]

{ #category : #settings }
MiniWorld >> displayScaleFactor [

	^ WorldMorph displayScaleFactor
]

{ #category : #display }
MiniWorld >> displayWorld [

	worldState displayWorld: self
]

{ #category : #'world state' }
MiniWorld >> displayWorldSafely [

	worldState displayWorldSafely: self
]

{ #category : #display }
MiniWorld >> doOneCycle [

	self doOneCycleNow
]

{ #category : #display }
MiniWorld >> doOneCycleNow [
	"see the comment in doOneCycleNowFor:"

	worldState doOneCycleFor: self
]

{ #category : #display }
MiniWorld >> fullRepaintNeeded [

	worldState doFullRepaint.
	self windowsSatisfying: [ :w |
		w makeMeVisible.
		false ]
]

{ #category : #'world state' }
MiniWorld >> handsDo: aBlock [

	^ worldState handsDo: aBlock
]

{ #category : #initialization }
MiniWorld >> initialize [

	super initialize.
	submorphs := OrderedCollection new.
	worldState := WorldState new.
	self color: Color veryLightGray
]

{ #category : #'change reporting' }
MiniWorld >> invalidRect: damageRect from: aMorph [
	"Clip damage reports to my bounds, since drawing is clipped to my bounds."

	worldState recordDamagedRect:
		(damageRect intersect: self bounds ifNone: [ ^ self ])
]

{ #category : #testing }
MiniWorld >> isCursorOwner [

	self currentCursor ifNotNil: [ ^ true ].
	^ false
]

{ #category : #testing }
MiniWorld >> isMiniWorld [

	^ true
]

{ #category : #setter }
MiniWorld >> manager: aWindowManager [

	manager := aWindowManager
]

{ #category : #'world simulation' }
MiniWorld >> menubar [

	^ self submorphs detect: #isMenubar
]

{ #category : #modal }
MiniWorld >> modalLockTo: aSystemWindow [
	"Lock the receiver as a modal owner of the given window."

	aSystemWindow setProperty: #modalOwner toValue: self.
	self setProperty: #modalChild toValue: aSystemWindow
]

{ #category : #modal }
MiniWorld >> modalUnlockFrom: aSystemWindow [
	"Unlock the receiver as a modal owner of the given window."

	aSystemWindow removeProperty: #modalOwner.
	self removeProperty: #modalChild.
	self removeProperty: #preModalCloseEnabled.
	self activate
]

{ #category : #'API - opening' }
MiniWorld >> open [

	| worldRenderer |
	worldRenderer := OSWorldRenderer forWorld: self.
	worldRenderer windowCloseAction: [ self worldRenderer deactivate ].
	worldState activeHand: (self addHand: HandMorph new).
	WorldMorph addExtraWorld: self.
	worldRenderer osWindow announcer weak
		when: DeleteWindowByCrossAnnouncement
		send: #deleteByCross
		to: self
]

{ #category : #'API - opening' }
MiniWorld >> openInWindow: aMorph [

	manager ifNotNil: [
		manager openNewWindowWithRoot: aMorph.
		^ self ].
	aMorph openInExternalWindow
]

{ #category : #'API - opening' }
MiniWorld >> openModal: aSystemWindow [
	"Open the given window locking the receiver until it is dismissed.
	Answer the system window.
	Restore the original keyboard focus when closed."

	| keyboardFocus |
	keyboardFocus := self activeHand keyboardFocus.
	self modalLockTo: aSystemWindow.
	[ aSystemWindow openModalInWorld: self ] ensure: [
		self modalUnlockFrom: aSystemWindow.
		self activeHand newKeyboardFocus: keyboardFocus ].
	^ aSystemWindow
]

{ #category : #'API - opening' }
MiniWorld >> openWithAttributes: aAttributes [

	| worldRenderer |
	worldRenderer := OSWorldRenderer
		                 forWorld: self
		                 withAttributs: aAttributes.
	self worldState worldRenderer:
		(worldRenderer windowCloseAction: [ self worldRenderer deactivate ]).
	worldState activeHand: (self addHand: HandMorph new).
	WorldMorph addExtraWorld: self.
	worldRenderer osWindow announcer weak
		when: DeleteWindowByCrossAnnouncement
		send: #deleteByCross
		to: self
]

{ #category : #'world state' }
MiniWorld >> removeAlarm: aSelector for: aTarget [

	^ worldState removeAlarm: aSelector for: aTarget
]

{ #category : #display }
MiniWorld >> resizeBackgroundMorph [
	"Resize the background morph to fit the world."

	self backgroundMorph ifNotNil: [
		self backgroundMorph extent: self extent ]
]

{ #category : #display }
MiniWorld >> restoreMorphicDisplay [

	worldState worldRenderer restoreMorphicDisplay
]

{ #category : #'world state' }
MiniWorld >> runLocalStepMethods [

	worldState runLocalStepMethodsIn: self
]

{ #category : #'world state' }
MiniWorld >> runStepMethods [

	worldState runStepMethodsIn: self
]

{ #category : #setter }
MiniWorld >> setLabel: aString [

	
]

{ #category : #stepping }
MiniWorld >> startStepping: aMorph [

	self
		startStepping: aMorph
		at: Time millisecondClockValue
		selector: #stepAt:
		arguments: nil
		stepTime: nil
]

{ #category : #stepping }
MiniWorld >> startStepping: aMorph at: scheduledTime selector: aSelector arguments: args stepTime: stepTime [

	worldState
		startStepping: aMorph
		at: scheduledTime
		selector: aSelector
		arguments: args
		stepTime: stepTime
]

{ #category : #stepping }
MiniWorld >> step [

	owner ifNotNil: [ ^ self runLocalStepMethods ].
	^ super step
]

{ #category : #stepping }
MiniWorld >> stopStepping: aMorph [

	worldState stopStepping: aMorph
]

{ #category : #stepping }
MiniWorld >> stopStepping: aMorph selector: aSelector [

	worldState stopStepping: aMorph selector: aSelector
]

{ #category : #accessing }
MiniWorld >> viewBox [

	^ self worldRenderer viewBox
]

{ #category : #'world state' }
MiniWorld >> viewBox: newViewBox [
	"I am now displayed within newViewBox; react."

	(self viewBox isNil or: [ self viewBox extent ~= newViewBox extent ])
		ifTrue: [ worldState invalidate ].

	self position: newViewBox topLeft.
	fullBounds := bounds := newViewBox.

	worldState handsDo: [ :hand | hand releaseKeyboardFocus ].
	self fullRepaintNeeded
]

{ #category : #accessing }
MiniWorld >> window [

	^ self
]

{ #category : #'world state' }
MiniWorld >> windowsSatisfying: windowBlock [

	| windows |
	windows := OrderedCollection new.
	self submorphs do: [ :m |
		m embeddedWindowOrNil ifNotNil: [
			(windowBlock value: m) ifTrue: [ windows addLast: m ] ] ].

	^ windows
]

{ #category : #accessing }
MiniWorld >> world [

	^ self
]

{ #category : #accessing }
MiniWorld >> worldRenderer [

	^ worldState worldRenderer
]

{ #category : #accessing }
MiniWorld >> worldState [

	^ worldState
]
