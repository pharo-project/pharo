"
A WorldMorph is a special morph that represents the world.
It is a paste up and it holds active hand.

A World, the entire Smalltalk screen, is a PasteUpMorph.  A World responds true to #isWorld.
"
Class {
	#name : 'WorldMorph',
	#superclass : 'PasteUpMorph',
	#instVars : [
		'zoomFactor'
	],
	#classVars : [
		'AllowDropFiles',
		'CursorOwnerWorld',
		'ExtraWorldList',
		'ExtraWorldListMutex',
		'LastScreenModeSelected'
	],
	#classInstVars : [
		'displayScaleFactor'
	],
	#category : 'Morphic-Core-Worlds',
	#package : 'Morphic-Core',
	#tag : 'Worlds'
}

{ #category : 'extra worlds' }
WorldMorph class >> addExtraWorld: aWorld [
	ExtraWorldListMutex critical: [
		ExtraWorldList := self extraWorldList copyWith: aWorld
	]
]

{ #category : 'accessing' }
WorldMorph class >> allowDropFiles [
	"If this option is set to false, the world morph will no longer be able to receive droped files. This help to protect private code while deploying an application."

	^ AllowDropFiles ifNil: [ AllowDropFiles := true ]
]

{ #category : 'accessing' }
WorldMorph class >> allowDropFiles: anObject [
	AllowDropFiles := anObject
]

{ #category : 'startup - shutdown' }
WorldMorph class >> cleanUp [
	self currentWorld cleanseOtherworldlySteppers
]

{ #category : 'extra worlds' }
WorldMorph class >> cursorOwnerWorld [
	^ CursorOwnerWorld ifNil: [ self currentWorld ]
]

{ #category : 'extra worlds' }
WorldMorph class >> cursorOwnerWorld: aWorld [

	CursorOwnerWorld := aWorld
]

{ #category : 'setting' }
WorldMorph class >> defaultWorldColor [

	^ Color r: 0.937 g: 0.937 b: 0.937
]

{ #category : 'setting' }
WorldMorph class >> displayScaleFactor [

	^ displayScaleFactor ifNil: [ displayScaleFactor := 1 ]
]

{ #category : 'setting' }
WorldMorph class >> displayScaleFactor: aNumber [

	displayScaleFactor := aNumber
]

{ #category : 'settings' }
WorldMorph class >> displaySettingsOn: aBuilder [
	<systemsettings>
	(aBuilder setting: #displayFullscreen)
		label: 'Fullscreen mode';
		parent: #desktopSettings;
		target: #WorldMorph;
		getSelector: #isFullscreen;
		setSelector: #fullscreen:;
		description: 'On platforms that support it, set fullscreen mode';
		default: false
]

{ #category : 'updating' }
WorldMorph class >> doOneCycle [

	WorldState doDrawCycleWith: [
		| extraWorldsToDraw |
		extraWorldsToDraw := ExtraWorldListMutex critical: [
			                     self extraWorldList ].
		extraWorldsToDraw do: [ :world | world doOneCycle ].

		(self currentWorld isNotNil and: [
			 (extraWorldsToDraw includes: self currentWorld) not ]) ifTrue: [
			self currentWorld doOneCycle ] ]
]

{ #category : 'extra worlds' }
WorldMorph class >> extraWorldList [
	ExtraWorldList ifNil: [ ExtraWorldList := #() ].
	^ ExtraWorldList
]

{ #category : 'settings' }
WorldMorph class >> fullscreen: aBoolean [

	self class codeSupportAnnouncer announce:
		(FullscreenAnnouncement new fullscreen:
			 (LastScreenModeSelected := aBoolean))
]

{ #category : 'class initialization' }
WorldMorph class >> initialize [
	ExtraWorldListMutex := Semaphore forMutualExclusion
]

{ #category : 'settings' }
WorldMorph class >> isFullscreen [

	^ LastScreenModeSelected ifNil: [ LastScreenModeSelected := false ]
]

{ #category : 'extra worlds' }
WorldMorph class >> removeExtraWorld: aWorld [
	ExtraWorldListMutex critical: [
		ExtraWorldList := self extraWorldList copyWithout: aWorld.
	].
	CursorOwnerWorld == aWorld ifTrue: [ CursorOwnerWorld := nil ]
]

{ #category : 'setting' }
WorldMorph class >> scaleFactor [

	self
		deprecated: 'Use #zoomFactor instead.'
		transformWith: '`@rcv scaleFactor' -> '`@rcv zoomFactor'.
	^ self zoomFactor
]

{ #category : 'setting' }
WorldMorph class >> scaleFactor: aValue [

	self
		deprecated: 'Use #zoomFactor: instead.'
		transformWith: '`@rcv scaleFactor: `@arg1' -> '`@rcv zoomFactor: `@arg1'.
	^ self zoomFactor: aValue
]

{ #category : 'settings' }
WorldMorph class >> toggleFullscreen [
	self fullscreen: self isFullscreen not
]

{ #category : 'setting' }
WorldMorph class >> zoomFactor [

	^ World zoomFactor
]

{ #category : 'setting' }
WorldMorph class >> zoomFactor: aValue [

	World zoomFactor: aValue
]

{ #category : 'setting' }
WorldMorph class >> zoomFactorSettingsOn: aBuilder [
	<systemsettings>

	(aBuilder pickOne: #zoomFactor)
		parent: #appearance;
		order: 3;
		default: 1;
		label: 'Zoom factor';
		description: 'Specify zoom factor for UI elements. Bigger zooms in. Smaller zooms out.';
		target: self;
		domainValues: (#( 1 1.5 2 2.5 3 3.5 4 4.5 5 ) collect: [ :e | 
			(e * 100) asInteger asString, '%' -> e ])
]

{ #category : 'operations' }
WorldMorph >> activateCursor: aCursor withMask: maskForm [

	worldState worldRenderer activateCursor: aCursor withMask: maskForm
]

{ #category : 'structure' }
WorldMorph >> activeHand [

	^ worldState activeHand
]

{ #category : 'world state' }
WorldMorph >> activeHand: aHandMorph [
	"Temporarily retained for old main event loops"
	worldState activeHand: aHandMorph
]

{ #category : 'accessing' }
WorldMorph >> actualScreenSize [

	^ self worldState worldRenderer actualScreenSize
]

{ #category : 'alarms - scheduler' }
WorldMorph >> addAlarm: aSelector withArguments: argArray for: aTarget at: scheduledTime [
	"Add a new alarm with the given set of parameters"
	worldState addAlarm: aSelector withArguments: argArray for: aTarget at: scheduledTime
]

{ #category : 'submorphs - add/remove' }
WorldMorph >> addAllMorphs: array [

	super addAllMorphs: array.
	array do: [ :m | m startSteppingSubmorphs ]
]

{ #category : 'world state' }
WorldMorph >> addHand: aHandMorph [
	"Add the given hand to the list of hands for this world."

	aHandMorph owner ifNotNil: [ aHandMorph owner removeHand: aHandMorph ].
	worldState addHand: aHandMorph.
	aHandMorph privateOwner: self
]

{ #category : 'menus' }
WorldMorph >> addStandardHaloMenuItemsTo: aMenu hand: aHandMorph [
	"Add standard halo items to the menu"

	self addWorldHaloMenuItemsTo: aMenu hand: aHandMorph
]

{ #category : 'submorphs - accessing' }
WorldMorph >> allMorphsDo: aBlock [
	"Enumerate all morphs in the world, including those held in hands."

	super allMorphsDo: aBlock.
	worldState handsReverseDo: [ :h | h allMorphsDo: aBlock ]
]

{ #category : 'accessing' }
WorldMorph >> announcer [
	WorldAnnouncer ifNil: [ WorldAnnouncer := Announcer new ].
	^ WorldAnnouncer
]

{ #category : 'cursor' }
WorldMorph >> beCursorOwner [
	self class cursorOwnerWorld: self
]

{ #category : 'initialization' }
WorldMorph >> becomeActiveDuring: aBlock [
	"Make the receiver the ActiveWorld during the evaluation of aBlock.
	Note that this method does deliberately *not* use #ensure: to prevent
	re-installation of the world on project switches."

	| priorWorld |
	priorWorld := ActiveWorld.
	ActiveWorld := self.
	aBlock
		on: Error
		do: [ :ex |
			ActiveWorld := priorWorld.
			ex pass ].
	ActiveWorld := priorWorld
]

{ #category : 'meta-actions' }
WorldMorph >> buildMetaMenu: evt [
	| menu |
	menu := self morphicUIManager newMenuIn: self for: self.
	self addStandardHaloMenuItemsTo: menu hand: evt hand.
	^ menu
]

{ #category : 'stepping' }
WorldMorph >> cleanseStepList [
	"Remove morphs from the step list that are not in this World."

	worldState cleanseStepListForWorld: self
]

{ #category : 'menu & halo' }
WorldMorph >> contentsMenu: aMenu [
	"Build the menu used from PopUpContentsMenu:"

	| expanded collapsed |
	expanded := self windowsSatisfying: [ :w | w isCollapsed not ].
	collapsed := self windowsSatisfying: [ :w | w isCollapsed ].
	(expanded asSortedCollection: [ :w1 :w2 | w1 label caseInsensitiveLessOrEqual: w2 label ])
		do: [ :w |
			aMenu add: (self truncatedMenuLabelFor: w label) target: w selector: #activateAndForceLabelToShow.
			aMenu lastItem iconFormSet: (self iconFormSetNamed: w taskbarIconName).
			w model canDiscardEdits
				ifFalse: [ aMenu lastItem color: Color red ] ].
	aMenu addLine.
	(collapsed asSortedCollection: [ :w1 :w2 | w1 label caseInsensitiveLessOrEqual: w2 label ])
		do: [ :w |
			aMenu add: (self truncatedMenuLabelFor: w label) target: w selector: #collapseOrExpand.
			aMenu lastItem iconFormSet: (self iconFormSetNamed: w taskbarIconName).
			w model canDiscardEdits
				ifFalse: [ aMenu lastItem color: Color red ] ].
	aMenu addLine.
	((self submorphs reject: [ :sm | (expanded includes: sm) or: [ collapsed includes: sm ] ])
		asSortedCollection: [ :w1 :w2 | w1 class name caseInsensitiveLessOrEqual: w2 class name ])
		do: [ :w |
			aMenu add: (self truncatedMenuLabelFor: w class name) target: w selector: #comeToFront.
			aMenu lastItem iconFormSet: (self iconFormSetNamed: w taskbarIconName) ].

	^ aMenu
]

{ #category : 'menu & halo' }
WorldMorph >> contentsMenuTitle [
	^ 'World contents' translated
]

{ #category : 'screenshot' }
WorldMorph >> copyRectangle: aRectangle into: aForm [

	^ worldState worldRenderer copyRectangle: aRectangle into: aForm
]

{ #category : 'world state' }
WorldMorph >> defaultWorldColor [
	^ self class defaultWorldColor
]

{ #category : 'deferred message' }
WorldMorph >> defer: aValuable [
	"aValuable will be executed in the next UI rendering cycle"
	worldState defer: aValuable
]

{ #category : 'menu & halo' }
WorldMorph >> deleteBalloonTarget: aMorph [
	"Delete the balloon help targeting the given morph"

	self handsDo: [ :h | h deleteBalloonTarget: aMorph ]
]

{ #category : 'accessing' }
WorldMorph >> depth [
	
	^ self worldState worldRenderer depth
]

{ #category : 'world menu' }
WorldMorph >> discoveredWorldMenu [
	^ worldState discoveredWorldMenu
]

{ #category : 'api' }
WorldMorph >> display [

	^ self worldState worldRenderer display
]

{ #category : 'accessing' }
WorldMorph >> displayArea [

	^ self worldState worldRenderer usableArea
]

{ #category : 'geometry' }
WorldMorph >> displayScaleFactor [

	^ self class displayScaleFactor
]

{ #category : 'world state' }
WorldMorph >> displayWorld [

	worldState displayWorld: self
]

{ #category : 'world state' }
WorldMorph >> displayWorldSafely [
	worldState displayWorldSafely: self
]

{ #category : 'world state' }
WorldMorph >> doOneCycle [
	self doOneCycleNow
]

{ #category : 'interaction loop' }
WorldMorph >> doOneCycleNow [
	"see the comment in doOneCycleNowFor:"
	worldState doOneCycleFor: self
]

{ #category : 'geometry' }
WorldMorph >> extent: aPoint [
	super extent: aPoint.
	worldState viewBox ifNotNil: [
		worldState invalidate ]
]

{ #category : 'project state' }
WorldMorph >> firstHand [
	^ worldState hands first
]

{ #category : 'geometry - testing' }
WorldMorph >> fullContainsPoint: pt [
	^bounds containsPoint: pt
]

{ #category : 'Morphic-Base-Windows' }
WorldMorph >> fullRepaintNeeded [

	worldState doFullRepaint.
	self windowsSatisfying: [:w | w makeMeVisible. false]
]

{ #category : 'event handling' }
WorldMorph >> fullscreenChanged: fullscreenAnnouncement [

	self worldState worldRenderer fullscreenMode: fullscreenAnnouncement fullscreen
]

{ #category : 'world state' }
WorldMorph >> haloMorphs [

	^ self hands collect: [ :h | h halo ] thenSelect: [ :halo | halo isNotNil ]
]

{ #category : 'project state' }
WorldMorph >> hands [

	^ worldState hands
]

{ #category : 'project state' }
WorldMorph >> handsDo: aBlock [

	^  worldState handsDo: aBlock
]

{ #category : 'project state' }
WorldMorph >> handsReverseDo: aBlock [
	^ worldState handsReverseDo: aBlock
]

{ #category : 'initialization' }
WorldMorph >> initialize [

	worldState := WorldState new.
	super initialize.

	self class codeSupportAnnouncer weak when: FullscreenAnnouncement send: #fullscreenChanged: to: self
]

{ #category : 'change reporting' }
WorldMorph >> invalidRect: damageRect from: aMorph [
	"Clip damage reports to my bounds, since drawing is clipped to my bounds."

	worldState recordDamagedRect: (damageRect intersect: self bounds ifNone: [ ^ self ])
]

{ #category : 'cursor' }
WorldMorph >> isCursorOwner [
	^ self class cursorOwnerWorld == self
]

{ #category : 'world menu' }
WorldMorph >> isEasySelecting [
	"This is to isolate easySelection predicate.
	Selectors in holders make no sense so we are limiting easy
	selection to the worldMorph.
	It would also make sense in playfield so feel free to adjust this
	predicate. Selection can always be forced by using the shift
	before mouse down."
	^ worldState isEasySelecting
]

{ #category : 'project state' }
WorldMorph >> isStepping: aMorph [
	^ worldState isStepping: aMorph
]

{ #category : 'project state' }
WorldMorph >> isStepping: aMorph selector: aSelector [
	^ worldState isStepping: aMorph selector: aSelector
]

{ #category : 'classification' }
WorldMorph >> isWorldMorph [

	^ true
]

{ #category : 'project state' }
WorldMorph >> listOfSteppingMorphs [
	^ worldState listOfSteppingMorphs

"self currentWorld listOfSteppingMorphs"
]

{ #category : 'printing' }
WorldMorph >> printOn: aStream [
	"Reimplemented to add a tag showing that the receiver is currently functioning as a 'world', if it is"

	super printOn: aStream.
	aStream nextPutAll: ' [world]'
]

{ #category : 'private' }
WorldMorph >> privateMoveBy: delta [
	super privateMoveBy: delta
]

{ #category : 'event handling' }
WorldMorph >> releaseCachedState [
	super releaseCachedState.
	self cleanseStepList
]

{ #category : 'alarms - scheduler' }
WorldMorph >> removeAlarm: aSelector for: aTarget [
	"Remove the alarm with the given selector"

	worldState removeAlarm: aSelector for: aTarget
]

{ #category : 'world state' }
WorldMorph >> removeHand: aHandMorph [
	"Remove the given hand from the list of hands for this world."

	(worldState hands includes: aHandMorph)
		ifFalse: [ ^ self ].
	aHandMorph dropMorphs.
	self invalidRect: aHandMorph fullBounds.
	worldState removeHand: aHandMorph
]

{ #category : 'event handling' }
WorldMorph >> requestStopTextEditing [

	self worldState worldRenderer requestStopTextEditing
]

{ #category : 'event handling' }
WorldMorph >> requestTextEditingAt: aRectangle [

	self worldState worldRenderer requestTextEditingAt: aRectangle
]

{ #category : 'world state' }
WorldMorph >> restoreMorphicDisplay [

	worldState worldRenderer restoreMorphicDisplay
]

{ #category : 'stepping' }
WorldMorph >> runLocalStepMethods [

	worldState runLocalStepMethodsIn: self
]

{ #category : 'stepping' }
WorldMorph >> runStepMethods [

	worldState runStepMethodsIn: self
]

{ #category : 'accessing' }
WorldMorph >> scaleFactor: newZoomFactor [

	self
		deprecated: 'Use #zoomFactor: instead'
		on: '20/12/2024'
		in: 'pharo13'
		transformWith:
		'`@receiver scaleFactor: `@arg' -> '`@receiver zoomFactor: `@arg'.
	self zoomFactor: newZoomFactor
]

{ #category : 'stepping' }
WorldMorph >> startStepping: aMorph [
	"Add the given morph to the step list. Do nothing if it is already being stepped."
	^self startStepping: aMorph at: Time millisecondClockValue selector: #stepAt: arguments: nil stepTime: nil
]

{ #category : 'stepping' }
WorldMorph >> startStepping: aMorph at: scheduledTime selector: aSelector arguments: args stepTime: stepTime [
	worldState startStepping: aMorph at: scheduledTime selector: aSelector arguments: args stepTime: stepTime
]

{ #category : 'stepping' }
WorldMorph >> step [
	owner ifNotNil: [ ^ self runLocalStepMethods ].
	^ super step
]

{ #category : 'project state' }
WorldMorph >> stepListSize [
	^ worldState stepListSize

"Transcript cr; show: self currentWorld stepListSize printString, ' items on steplist as of ', Date dateAndTimeNow printString"
]

{ #category : 'project state' }
WorldMorph >> steppingMorphsNotInWorld [
	| all |
	all := self allMorphs.
	^ self listOfSteppingMorphs reject: [ :m | all includes: m ]	"self currentWorld steppingMorphsNotInWorld do: [:m | m delete]"
]

{ #category : 'stepping' }
WorldMorph >> stopStepping: aMorph [
	"Remove the given morph from the step list."

	worldState stopStepping: aMorph
]

{ #category : 'stepping' }
WorldMorph >> stopStepping: aMorph selector: aSelector [
	"Remove the given morph from the step list."

	worldState stopStepping: aMorph selector: aSelector
]

{ #category : 'menu & halo' }
WorldMorph >> truncatedMenuLabelFor: aWindowLabel [
	^ aWindowLabel truncateWithElipsisTo: 47
]

{ #category : 'copying' }
WorldMorph >> veryDeepCopyWith: deepCopier [
	"never copy the World"

	^ self
]

{ #category : 'project state' }
WorldMorph >> viewBox [

	^ worldState viewBox
]

{ #category : 'world state' }
WorldMorph >> viewBox: newViewBox [
	"I am now displayed within newViewBox; react."

	(self viewBox isNil or: [ self viewBox extent ~= newViewBox extent ])
		ifTrue: [ worldState invalidate ].

	super viewBox: newViewBox.
	worldState handsDo: [ :hand | hand releaseKeyboardFocus ].
	self fullRepaintNeeded
]

{ #category : 'halos and balloon help' }
WorldMorph >> wantsDirectionHandles [

	^ false
]

{ #category : 'event handling' }
WorldMorph >> wantsDropFiles: anEvent [
	"We check if the WorldMorph is configured to be able to receive droped files. During the deployment of an application this option might be disabled."

	^ self class allowDropFiles
]

{ #category : 'private' }
WorldMorph >> wantsDroppedMorph: aMorph event: evt [

	^ true
]

{ #category : 'menu & halo' }
WorldMorph >> wantsWindowEvent: anEvent [
	^ true
]

{ #category : 'structure' }
WorldMorph >> world [

	^self
]

{ #category : 'world menu' }
WorldMorph >> worldMenu [
	^ worldState worldMenu
]

{ #category : 'accessing' }
WorldMorph >> worldState [

	^ worldState
]

{ #category : 'accessing' }
WorldMorph >> zoomFactor [

	^ zoomFactor
]

{ #category : 'accessing' }
WorldMorph >> zoomFactor: newScaleFactor [
	
	zoomFactor := newScaleFactor
]
