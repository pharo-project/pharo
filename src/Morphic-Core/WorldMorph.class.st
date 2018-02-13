"
A WorldMorph is a special morph that represents the world.
It is a paste up and it holds active hand.

A World, the entire Smalltalk screen, is a PasteUpMorph.  A World responds true to isWorld.
"
Class {
	#name : #WorldMorph,
	#superclass : #PasteUpMorph,
	#instVars : [
		'osWindow',
		'session'
	],
	#classVars : [
		'AllowDropFiles',
		'CursorOwnerWorld',
		'ExtraWorldList'
	],
	#category : #'Morphic-Core-Worlds'
}

{ #category : #'extra worlds' }
WorldMorph class >> addExtraWorld: aWorld [
	ExtraWorldList := self extraWorldList copyWith: aWorld
]

{ #category : #accessing }
WorldMorph class >> allowDropFiles [
	"If this option is set to false, the world morph will no longer be able to receive droped files. This help to protect private code while deploying an application."
	
	^ AllowDropFiles ifNil: [ AllowDropFiles := true ]
]

{ #category : #accessing }
WorldMorph class >> allowDropFiles: anObject [
	AllowDropFiles := anObject
]

{ #category : #'system startup' }
WorldMorph class >> cleanUp [
	World cleanseOtherworldlySteppers.
]

{ #category : #'extra worlds' }
WorldMorph class >> cursorOwnerWorld [
	^ CursorOwnerWorld ifNil: [ World ]
]

{ #category : #'extra worlds' }
WorldMorph class >> cursorOwnerWorld: aWorld [
	CursorOwnerWorld ~~ aWorld ifTrue: [ 
		CursorOwnerWorld := aWorld.
		(CursorOwnerWorld currentCursor ifNil: [Cursor normal]) beCursor
	]

]

{ #category : #setting }
WorldMorph class >> defaultWorldColor [

	^ Color r: 0.937 g: 0.937 b: 0.937.

]

{ #category : #'initialize-release' }
WorldMorph class >> doOneCycle [
	self extraWorldList do: [:world |
		ActiveHand := world activeHand.
		world doOneCycle.
	].

	(World isNotNil and: [(self extraWorldList includes: World) not]) ifTrue: [
		World doOneCycle.
		ActiveHand := World activeHand
	].
	
	
]

{ #category : #'extra worlds' }
WorldMorph class >> extraWorldList [
	ExtraWorldList ifNil: [ ExtraWorldList := #() ].
	^ ExtraWorldList
]

{ #category : #'initialize-release' }
WorldMorph class >> initialize [
	SessionManager default registerGuiClassNamed: self name.
]

{ #category : #'extra worlds' }
WorldMorph class >> removeExtraWorld: aWorld [
	ExtraWorldList := self extraWorldList copyWithout: aWorld.
	CursorOwnerWorld == aWorld ifTrue: [ CursorOwnerWorld := nil. ]
	
]

{ #category : #'system startup' }
WorldMorph class >> shutDown [
	World ifNotNil: [:world | world triggerEvent: #aboutToLeaveWorld ].
	self extraWorldList do: [:world | world triggerEvent: #aboutToLeaveWorld ].
]

{ #category : #'system startup' }
WorldMorph class >> startUp [
	World ifNotNil: [:world | world restoreMorphicDisplay].
	self extraWorldList do: #restoreMorphicDisplay.
]

{ #category : #structure }
WorldMorph >> activeHand [

	^ worldState activeHand
]

{ #category : #'world state' }
WorldMorph >> activeHand: aHandMorph [
	"temporarily retained for old main event loops"
	worldState activeHand: aHandMorph.


]

{ #category : #'alarms-scheduler' }
WorldMorph >> addAlarm: aSelector withArguments: argArray for: aTarget at: scheduledTime [
	"Add a new alarm with the given set of parameters"
	worldState addAlarm: aSelector withArguments: argArray for: aTarget at: scheduledTime.
]

{ #category : #'submorphs-add/remove' }
WorldMorph >> addAllMorphs: array [

	super addAllMorphs: array.
	array do: [:m | self startSteppingSubmorphsOf: m].

]

{ #category : #'world state' }
WorldMorph >> addHand: aHandMorph [
	"Add the given hand to the list of hands for this world."

	aHandMorph owner ifNotNil: [ aHandMorph owner removeHand: aHandMorph ].
	worldState addHand: aHandMorph.
	aHandMorph privateOwner: self
]

{ #category : #menus }
WorldMorph >> addStandardHaloMenuItemsTo: aMenu hand: aHandMorph [
	"Add standard halo items to the menu"

	self addWorldHaloMenuItemsTo: aMenu hand: aHandMorph
]

{ #category : #'submorphs-accessing' }
WorldMorph >> allMorphsDo: aBlock [
	"Enumerate all morphs in the world, including those held in hands."

	super allMorphsDo: aBlock.
	worldState handsReverseDo: [ :h | h allMorphsDo: aBlock ]
]

{ #category : #accessing }
WorldMorph >> announcer [
	WorldAnnouncer ifNil: [ WorldAnnouncer := Announcer new ].
	^ WorldAnnouncer
]

{ #category : #'world state' }
WorldMorph >> assuredCanvas [
	^ worldState assuredCanvas
]

{ #category : #cursor }
WorldMorph >> beCursorOwner [
	self class cursorOwnerWorld: self
]

{ #category : #'meta-actions' }
WorldMorph >> buildMetaMenu: evt [
	| menu |
	menu := UIManager default newMenuIn: self for: self.
	self addStandardHaloMenuItemsTo: menu hand: evt hand.
	^ menu

]

{ #category : #'project state' }
WorldMorph >> canvas [

	^ worldState canvas
]

{ #category : #stepping }
WorldMorph >> cleanseStepList [
	"Remove morphs from the step list that are not in this World."

	worldState cleanseStepListForWorld: self
]

{ #category : #'menu & halo' }
WorldMorph >> contentsMenu: aMenu [
	"Build the menu used from PopUpContentsMenu:"

	| expanded collapsed |
	expanded := self windowsSatisfying: [ :w | w isCollapsed not ].
	collapsed := self windowsSatisfying: [ :w | w isCollapsed ].
	(expanded asSortedCollection: [ :w1 :w2 | w1 label caseInsensitiveLessOrEqual: w2 label ])
		do: [ :w | 
			aMenu add: (self truncatedMenuLabelFor: w label) target: w selector: #activateAndForceLabelToShow.
			aMenu lastItem icon: w taskbarIcon.
			w model canDiscardEdits 
				ifFalse: [ aMenu lastItem color: Color red ] ].
	aMenu addLine.
	(collapsed asSortedCollection: [ :w1 :w2 | w1 label caseInsensitiveLessOrEqual: w2 label ])
		do: [ :w | 
			aMenu add: (self truncatedMenuLabelFor: w label) target: w selector: #collapseOrExpand.
			aMenu lastItem icon: w taskbarIcon.
			w model canDiscardEdits
				ifFalse: [ aMenu lastItem color: Color red ] ].
	aMenu addLine.
	((self submorphs reject: [ :sm | (expanded includes: sm) or: [ collapsed includes: sm ] ])
		asSortedCollection: [ :w1 :w2 | w1 class name caseInsensitiveLessOrEqual: w2 class name ])
		do: [ :w | 
			aMenu add: (self truncatedMenuLabelFor: w class name) target: w selector: #comeToFront.
			aMenu lastItem icon: w taskbarIcon ].
		
	^ aMenu
]

{ #category : #'menu & halo' }
WorldMorph >> contentsMenuTitle [
	^ 'World contents' translated
]

{ #category : #'world state' }
WorldMorph >> defaultWorldColor [
	^ self class defaultWorldColor
]

{ #category : #'deferred message' }
WorldMorph >> defer: aValuable [
	"aValuable will be executed in the next UI rendering cycle"
	worldState defer: aValuable
]

{ #category : #'menu & halo' }
WorldMorph >> deleteBalloonTarget: aMorph [
	"Delete the balloon help targeting the given morph"

	self handsDo: [ :h | h deleteBalloonTarget: aMorph ]
]

{ #category : #'world menu' }
WorldMorph >> discoveredWorldMenu [
	^ worldState discoveredWorldMenu
]

{ #category : #'world state' }
WorldMorph >> displayWorld [
	worldState displayWorld: self submorphs: submorphs
]

{ #category : #'world state' }
WorldMorph >> displayWorldSafely [
	worldState displayWorldSafely: self.

]

{ #category : #'world state' }
WorldMorph >> doOneCycle [
	"see the comment in doOneCycleFor:"

	worldState doOneCycleFor: self
]

{ #category : #'interaction loop' }
WorldMorph >> doOneCycleNow [
	"see the comment in doOneCycleNowFor:"
	worldState doOneCycleNowFor: self.

]

{ #category : #'world state' }
WorldMorph >> doOneSubCycle [
	"Like doOneCycle, but preserves activeHand."

	worldState doOneSubCycleFor: self
]

{ #category : #geometry }
WorldMorph >> extent: aPoint [
	super extent: aPoint.
	worldState viewBox
		ifNotNil: [ 
			worldState canvas: nil.
			worldState viewBox: bounds ]
]

{ #category : #'project state' }
WorldMorph >> firstHand [
	^ worldState hands first
]

{ #category : #'geometry testing' }
WorldMorph >> fullContainsPoint: pt [
	^bounds containsPoint: pt

]

{ #category : #'Morphic-Base-Windows' }
WorldMorph >> fullRepaintNeeded [

	worldState doFullRepaint.
	self  windowsSatisfying: [:w | w makeMeVisible. false].


]

{ #category : #'world state' }
WorldMorph >> haloMorphs [

	^ self hands collect: [ :h | h halo ] thenSelect: [ :halo | halo notNil ]
]

{ #category : #'project state' }
WorldMorph >> hands [

	^ worldState hands
]

{ #category : #'project state' }
WorldMorph >> handsDo: aBlock [

	^  worldState handsDo: aBlock 
]

{ #category : #'project state' }
WorldMorph >> handsReverseDo: aBlock [
	^ worldState handsReverseDo: aBlock
]

{ #category : #initialization }
WorldMorph >> initialize [ 
	worldState := WorldState new.
 	super initialize
]

{ #category : #'change reporting' }
WorldMorph >> invalidRect: damageRect from: aMorph [
	"Clip damage reports to my bounds, since drawing is clipped to my bounds."

	worldState recordDamagedRect: (damageRect intersect: self bounds ifNone: [ ^ self ])
]

{ #category : #cursor }
WorldMorph >> isCursorOwner [
	^ self class cursorOwnerWorld == self
]

{ #category : #'world menu' }
WorldMorph >> isEasySelecting [
	"This is to isolate easySelection predicate. 
	Selectors in holders make no sense so we are limiting easy
	selection to the worldMorph.
	It would also make sense in playfield so feel free to adjust this
	predicate. Selection can always be forced by using the shift
	before mouse down."
	^ worldState isEasySelecting
]

{ #category : #'project state' }
WorldMorph >> isStepping: aMorph [
	^ worldState isStepping: aMorph
]

{ #category : #'project state' }
WorldMorph >> isStepping: aMorph selector: aSelector [
	^ worldState isStepping: aMorph selector: aSelector
]

{ #category : #classification }
WorldMorph >> isWorldMorph [

	^ true
]

{ #category : #'project state' }
WorldMorph >> listOfSteppingMorphs [
	^ worldState listOfSteppingMorphs

"self currentWorld listOfSteppingMorphs"
]

{ #category : #'dropping/grabbing' }
WorldMorph >> mouseDown: evt [
	super mouseDown: evt.
	self currentWindow ifNotNil: [ :topWindow | SystemWindow passivateTopWindow ]
]

{ #category : #geometry }
WorldMorph >> position: aPoint [
	"Prevent moving a world (e.g. via HandMorph>>specialGesture:)"

	super position: aPoint.
	self viewBox ifNotNil: [:viewBox | self viewBox: (aPoint extent:  viewBox extent)].


]

{ #category : #printing }
WorldMorph >> printOn: aStream [
	"Reimplemented to add a tag showing that the receiver is currently functioning as a 'world', if it is"

	super printOn: aStream.
	aStream nextPutAll: ' [world]'
]

{ #category : #private }
WorldMorph >> privateMoveBy: delta [
	super privateMoveBy: delta.
	worldState viewBox ifNotNil: [ worldState viewBox: bounds ]
]

{ #category : #'event handling' }
WorldMorph >> releaseCachedState [
	super releaseCachedState.
	self cleanseStepList.
]

{ #category : #'alarms-scheduler' }
WorldMorph >> removeAlarm: aSelector for: aTarget [
	"Remove the alarm with the given selector"
	
	worldState removeAlarm: aSelector for: aTarget
]

{ #category : #'world state' }
WorldMorph >> removeHand: aHandMorph [
	"Remove the given hand from the list of hands for this world."

	(worldState hands includes: aHandMorph)
		ifFalse: [ ^ self ].
	aHandMorph dropMorphs.
	self invalidRect: aHandMorph fullBounds.
	worldState removeHand: aHandMorph
]

{ #category : #'world menu' }
WorldMorph >> resetWorldMenu [
	
	worldState resetWorldMenu

]

{ #category : #stepping }
WorldMorph >> runLocalStepMethods [

	worldState runLocalStepMethodsIn: self

]

{ #category : #stepping }
WorldMorph >> runStepMethods [

	worldState runStepMethodsIn: self

]

{ #category : #'world state' }
WorldMorph >> sleep [

	worldState canvas ifNil: [^ self  "already called (clean this up)"].
	Cursor normal show.	"restore the normal cursor"
	worldState canvas: nil.		"free my canvas to save space"
	self fullReleaseCachedState.

]

{ #category : #stepping }
WorldMorph >> startStepping: aMorph at: scheduledTime selector: aSelector arguments: args stepTime: stepTime [
	worldState startStepping: aMorph at: scheduledTime selector: aSelector arguments: args stepTime: stepTime.
]

{ #category : #stepping }
WorldMorph >> step [
	owner ifNotNil: [ ^ self runLocalStepMethods ].
	^ super step
]

{ #category : #'project state' }
WorldMorph >> stepListSize [
	^ worldState stepListSize

"Transcript cr; show: self currentWorld stepListSize printString, ' items on steplist as of ', Date dateAndTimeNow printString"
]

{ #category : #stepping }
WorldMorph >> stopStepping: aMorph [
	"Remove the given morph from the step list."

	worldState stopStepping: aMorph

]

{ #category : #stepping }
WorldMorph >> stopStepping: aMorph selector: aSelector [
	"Remove the given morph from the step list."

	worldState stopStepping: aMorph selector: aSelector

]

{ #category : #'menu & halo' }
WorldMorph >> truncatedMenuLabelFor: aWindowLabel [
	^ aWindowLabel truncateWithElipsisTo: 47
]

{ #category : #copying }
WorldMorph >> veryDeepCopyWith: deepCopier [
	"never copy the World"

	^ self
]

{ #category : #'project state' }
WorldMorph >> viewBox [

	^ worldState viewBox
]

{ #category : #'world state' }
WorldMorph >> viewBox: newViewBox [
	"I am now displayed within newViewBox; react."

	(self viewBox isNil or: [ self viewBox extent ~= newViewBox extent ])
		ifTrue: [ worldState canvas: nil ].
	worldState viewBox: newViewBox.
	super viewBox: newViewBox.
	worldState handsDo: [ :hand | hand releaseKeyboardFocus ].
	self fullRepaintNeeded
]

{ #category : #'halos and balloon help' }
WorldMorph >> wantsDirectionHandles [

	^ false
]

{ #category : #'event handling' }
WorldMorph >> wantsDropFiles: anEvent [
	"We check if the WorldMorph is configured to be able to receive droped files. During the deployment of an application this option might be disabled."
	
	^ self class allowDropFiles
]

{ #category : #private }
WorldMorph >> wantsDroppedMorph: aMorph event: evt [
	
	^ true
]

{ #category : #'menu & halo' }
WorldMorph >> wantsWindowEvent: anEvent [
	^ true
]

{ #category : #structure }
WorldMorph >> world [
	
	^self
]

{ #category : #'world menu' }
WorldMorph >> worldMenu [
	^ worldState worldMenu
	
]
