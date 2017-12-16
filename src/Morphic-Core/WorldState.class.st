"
The state of a Morphic world.  (This needs some serious commenting!!)


The MinCycleLapse variable holds the minimum amount of time that a morphic cycle is allowed to take.  If a cycle takes less than this, then interCyclePause: will wait until the full time has been used up.
"
Class {
	#name : #WorldState,
	#superclass : #Object,
	#instVars : [
		'hands',
		'viewBox',
		'canvas',
		'damageRecorder',
		'stepList',
		'lastStepTime',
		'lastStepMessage',
		'lastCycleTime',
		'alarms',
		'lastAlarmTime',
		'menuBuilder',
		'activeHand',
		'currentCursor'
	],
	#classVars : [
		'CanSurrenderToOS',
		'DebugShowDamage',
		'DeferredUIMessages',
		'DesktopMenuPragmaKeyword',
		'DesktopMenuTitle',
		'EasySelectingWorld',
		'LastCycleTime',
		'MinCycleLapse',
		'ServerMode',
		'ShowUpdateOptionInWorldMenu'
	],
	#category : #'Morphic-Core-Worlds'
}

{ #category : #accessing }
WorldState class >> MinCycleLapse: milliseconds [
	"set the minimum amount of time that may transpire between two calls to doOneCycle"
	MinCycleLapse := milliseconds ifNotNil: [ milliseconds rounded ].
]

{ #category : #'deferred message' }
WorldState class >> addDeferredUIMessage: aValuable [
	"aValuable will be executed in the next UI rendering cycle"
	self deferredUIMessages nextPut: aValuable


]

{ #category : #'testing os' }
WorldState class >> canSurrenderToOS: aBoolean [

	CanSurrenderToOS := aBoolean
]

{ #category : #accessing }
WorldState class >> debugShowDamage [
	^ DebugShowDamage ifNil: [DebugShowDamage := false]
]

{ #category : #accessing }
WorldState class >> debugShowDamage: aBoolean [
	DebugShowDamage := aBoolean
]

{ #category : #settings }
WorldState class >> defaultWorldMenu [
	self desktopMenuTitle: 'World'.
	self desktopMenuPragmaKeyword: 'worldMenu'.
]

{ #category : #'deferred message' }
WorldState class >> defer: aValuable [
	"aValuable will be executed in the next UI rendering cycle"
	self addDeferredUIMessage: aValuable.


]

{ #category : #'deferred message' }
WorldState class >> deferredUIMessages [

	^DeferredUIMessages ifNil: [DeferredUIMessages := WaitfreeQueue  new].

]

{ #category : #settings }
WorldState class >> desktopMenuPragmaKeyword: aString [
	DesktopMenuPragmaKeyword := aString.
	World resetWorldMenu.

]

{ #category : #settings }
WorldState class >> desktopMenuTitle: aString [
	DesktopMenuTitle := aString
]

{ #category : #settings }
WorldState class >> discoveredMenuPragmaKeyword [
	^ DesktopMenuPragmaKeyword ifNil: [DesktopMenuPragmaKeyword := 'worldMenu']
]

{ #category : #settings }
WorldState class >> discoveredMenuTitle [
	^ DesktopMenuTitle ifNil: [DesktopMenuTitle := 'World']
]

{ #category : #settings }
WorldState class >> easySelectingWorld [
	^ EasySelectingWorld ifNil: [EasySelectingWorld := false]
]

{ #category : #settings }
WorldState class >> easySelectingWorld: aBoolean [
	EasySelectingWorld := aBoolean
]

{ #category : #'world menu items' }
WorldState class >> helpOn: aBuilder [
	<worldMenu>
	(aBuilder item: #Help)
		order: 4.0;
		iconName: #smallHelpIcon;
		help: 'Some help tools on Pharo.';
		withSeparatorAfter
]

{ #category : #initialization }
WorldState class >> initialize [
	"WorldState initialize"

	MinCycleLapse := 20.		"allows 50 frames per second..."
	DeferredUIMessages := nil.
]

{ #category : #accessing }
WorldState class >> lastCycleTime [

	^LastCycleTime
]

{ #category : #'world menu items' }
WorldState class >> mostUsedToolsOn: aBuilder [
	<worldMenu>
	(aBuilder group: #MostUsedTools)
		withSeparatorAfter;
		order: 0
]

{ #category : #'world menu items' }
WorldState class >> quitItemsOn: aBuilder [
	<worldMenu>
	(aBuilder group: #QuitPharo)
		order: 9999;
		with: [ (aBuilder item: #Save)
				target: self;
				selector: #saveSession;
				help: 'Save the current version of the image on disk.';
				keyText: 'S';
				iconName: #smallSaveIcon.
			(aBuilder item: #'Save as...')
				target: self;
				selector: #saveAs;
				help: 'Save the current version of the image on disk under a new name.';
				iconName: #smallSaveAsIcon.
			(aBuilder item: #'Save and quit')
				target: self;
				selector: #saveAndQuit;
				help: 'Save the current image on disk, and quit Pharo.';
				iconName: #smallQuitIcon.
			(aBuilder item: #Quit)
				target: self;
				selector: #quitSession;
				help: 'Quit Pharo without saving the image on disk.';
				iconName: #smallQuitIcon ]
]

{ #category : #'world menu items' }
WorldState class >> quitSession [
	| save |
	save := UIManager default confirm: 'Save changes before quitting?' translated orCancel: [ ^ self ].
	save 
		ifTrue: [Smalltalk snapshot: true andQuit: true] 
		ifFalse: [Smalltalk snapshot: false andQuit: true]
 
]

{ #category : #'world menu items' }
WorldState class >> saveAndQuit [
	Cursor write showWhile: [
		Smalltalk snapshot: true andQuit: true
	]
]

{ #category : #'world menu items' }
WorldState class >> saveAs [
	| reference |
	reference := UIManager default 
		fileSave: 'Save Image as?' 
		initialAnswer: (Smalltalk imageFile nextVersion basename copyUpToLast: Path extensionDelimiter)
		extensions: #('image') 
		path: Smalltalk imageDirectory.
		
	reference ifNotNil: [ Smalltalk saveAs: reference parent / (reference basenameWithoutExtension: 'image') ]
]

{ #category : #'world menu items' }
WorldState class >> saveSession [
	Cursor write showWhile: [
		Smalltalk
			snapshot: true
			andQuit: false.
	]
]

{ #category : #'world menu items' }
WorldState class >> screenShotCommandOn: aBuilder [
	<worldMenu>
	(aBuilder item: #Screenshot)
		parent: #Tools;
		target: World;
		order: 0.59;
		selector: #makeAScreenshot;
		label: 'Screenshot';
		help: 'Take a screenshot of your Pharo image.';
		iconName: #smallScreenshotIcon
]

{ #category : #settings }
WorldState class >> serverMode [
	^ ServerMode ifNil: [ServerMode := false]
]

{ #category : #settings }
WorldState class >> serverMode: aBoolean [
	(aBoolean = true or: [aBoolean = false or: [aBoolean isNil]])
		ifTrue: [ServerMode := aBoolean]
]

{ #category : #settings }
WorldState class >> showUpdateOptionInWorldMenu [
	^ ShowUpdateOptionInWorldMenu ifNil: [ShowUpdateOptionInWorldMenu := true]
]

{ #category : #settings }
WorldState class >> showUpdateOptionInWorldMenu: aBoolean [
	ShowUpdateOptionInWorldMenu := aBoolean
]

{ #category : #accessing }
WorldState class >> theme [
	^ Smalltalk ui theme
]

{ #category : #hands }
WorldState >> activeHand [
	^ activeHand
]

{ #category : #hands }
WorldState >> activeHand: aHand [
	ActiveHand := aHand.
	activeHand := aHand
]

{ #category : #hands }
WorldState >> addHand: aHandMorph [
	"Add the given hand to the list of hands for this world."

	hands := (hands copyWithout: aHandMorph) copyWith: aHandMorph.

]

{ #category : #alarms }
WorldState >> adjustAlarmTimes: nowTime [
	"Adjust the alarm times after some clock weirdness (such as roll-over, image-startup etc)"
	| deltaTime |
	deltaTime := nowTime - lastAlarmTime.
	self alarms do:[:alarm| alarm scheduledTime: alarm scheduledTime + deltaTime].
]

{ #category : #stepping }
WorldState >> adjustWakeupTimes: now [
	"Fix the wakeup times in my step list. This is necessary when this world has been restarted after a pause, say because some other view had control, after a snapshot, or because the millisecond clock has wrapped around. (The latter is a rare occurence with a 32-bit clock!)"
	| deltaTime |
	deltaTime := now - lastStepTime.
	stepList do:[:entry| entry scheduledTime: entry scheduledTime + deltaTime].
	lastStepTime := now.

]

{ #category : #stepping }
WorldState >> adjustWakeupTimesIfNecessary [
	"Fix the wakeup times in my step list if necessary. This is needed after a snapshot, after a long pause (say because some other view had control or because the user was selecting from an MVC-style menu) or when the millisecond clock wraps around (a very rare occurence with a 32-bit clock!)."

	| now |
	now := Time millisecondClockValue.
	((now < lastStepTime) or: [(now - lastStepTime) > 5000])
		 ifTrue: [self adjustWakeupTimes: now].  "clock slipped"

]

{ #category : #alarms }
WorldState >> alarmSortBlock [
	^[ :alarm1 :alarm2 | 
		alarm1 scheduledTime < alarm2 scheduledTime.
	]
]

{ #category : #alarms }
WorldState >> alarms [

	^alarms ifNil: [alarms := Heap sortBlock: self alarmSortBlock]
]

{ #category : #canvas }
WorldState >> assuredCanvas [

	(canvas isNil or: [(canvas extent ~= viewBox extent) or: [canvas form depth ~= Display depth]])
		ifTrue:
			["allocate a new offscreen canvas the size of the window"
			self canvas: (Display defaultCanvasClass extent: viewBox extent)].
	^ self canvas
]

{ #category : #canvas }
WorldState >> canvas [

	^ canvas
]

{ #category : #canvas }
WorldState >> canvas: x [ 
	canvas := x.
	damageRecorder 
		ifNil: [damageRecorder := DamageRecorder new]
		ifNotNil: [damageRecorder doFullRepaint]
]

{ #category : #'update cycle' }
WorldState >> checkIfUpdateNeeded [

	damageRecorder updateIsNeeded ifTrue: [^true].
	hands do: [:h | (h hasChanged and: [h needsToBeDrawn]) ifTrue: [^true]].
	^false  "display is already up-to-date"

]

{ #category : #cleaning }
WorldState >> cleanStepList [
	"ensure that we have a cleaner block"
	^stepList sortBlock: self stepListSortBlock

]

{ #category : #stepping }
WorldState >> cleanseStepListForWorld: aWorld [
	"Remove morphs from the step list that are not in this World"

	| deletions morphToStep |
	deletions := nil.
	stepList do: [:entry |
		morphToStep := entry receiver.
		morphToStep world == aWorld ifFalse:[
			deletions ifNil: [deletions := OrderedCollection new].
			deletions addLast: entry]].

	deletions ifNotNil:[
		deletions do: [:entry|
			self stopStepping: entry receiver]].

	self alarms copy do:[:entry|
		morphToStep := entry receiver.
		(morphToStep isMorph and:[morphToStep world == aWorld]) 
			ifFalse:[self removeAlarm: entry selector for: entry receiver]].
]

{ #category : #'object filein' }
WorldState >> convertAlarms [
	
	alarms ifNotNil: [alarms sortBlock: self alarmSortBlock].	"ensure cleaner block"


]

{ #category : #accessing }
WorldState >> currentCursor [
	^ currentCursor
]

{ #category : #accessing }
WorldState >> currentCursor: anObject [
	currentCursor := anObject
]

{ #category : #'deferred message' }
WorldState >> defer: aValuable [
	"aValuable will be executed in the next UI rendering cycle"
	self class defer: aValuable
]

{ #category : #'worldmenu building' }
WorldState >> discoveredMenuOn: aBuilder [

	^ aBuilder menuEntitled: self discoveredMenuTitle
]

{ #category : #settings }
WorldState >> discoveredMenuPragmaKeyword [
	^ self class discoveredMenuPragmaKeyword
]

{ #category : #settings }
WorldState >> discoveredMenuTitle [
	^ self class discoveredMenuTitle
]

{ #category : #'update cycle' }
WorldState >> displayWorld: aWorld submorphs: submorphs [
	"Update this world's display."

	| deferredUpdateMode handsToDraw allDamage |

	submorphs do: [:m | m fullBounds].  "force re-layout if needed"
	self checkIfUpdateNeeded ifFalse: [^ self].  "display is already up-to-date"

	deferredUpdateMode := self doDeferredUpdatingFor: aWorld.
	deferredUpdateMode ifFalse: [self assuredCanvas].
	canvas roundCornersOf: aWorld during:[ | worldDamageRects handDamageRects |
		worldDamageRects := self drawWorld: aWorld submorphs: submorphs invalidAreasOn: canvas.  "repair world's damage on canvas"
		"self handsDo:[:h| h noticeDamageRects: worldDamageRects]."
		handsToDraw := self selectHandsToDrawForDamage: worldDamageRects.
		handDamageRects := handsToDraw collect: [:h | h savePatchFrom: canvas].
		allDamage := worldDamageRects, handDamageRects.

		handsToDraw reverseDo: [:h | canvas fullDrawMorph: h].  "draw hands onto world canvas"
	].
	"*make this true to flash damaged areas for testing*"
	self class debugShowDamage ifTrue: [aWorld flashRects: allDamage color: Color black].

	canvas finish.
	"quickly copy altered rects of canvas to Display:"
	deferredUpdateMode
		ifTrue: [self forceDamageToScreen: allDamage]
		ifFalse: [canvas showAt: aWorld viewBox origin invalidRects: allDamage].
	handsToDraw do: [:h | h restoreSavedPatchOn: canvas].  "restore world canvas under hands"
	Display deferUpdates: false; forceDisplayUpdate.

]

{ #category : #'update cycle' }
WorldState >> displayWorldSafely: aWorld [
	"Update this world's display and keep track of errors during draw methods."

	[aWorld displayWorld] ifError: [:err :rcvr |
		"Handle a drawing error"
		| errCtx errMorph |
		errCtx := thisContext.
		[
			errCtx := errCtx sender.
			"Search the sender chain to find the morph causing the problem"
			[errCtx notNil and:[(errCtx receiver isMorph) not]] 
				whileTrue:[errCtx := errCtx sender].
			"If we're at the root of the context chain then we have a fatal drawing problem"
			errCtx ifNil:[^self handleFatalDrawingError: err].
			errMorph := errCtx receiver.
			"If the morph causing the problem has already the #drawError flag set,
			then search for the next morph above in the caller chain."
			errMorph hasProperty: #errorOnDraw
		] whileTrue.
		errMorph setProperty: #errorOnDraw toValue: true.
		"Install the old error handler, so we can re-raise the error"
		rcvr error: err.
	].
]

{ #category : #'update cycle' }
WorldState >> doDeferredUpdatingFor: aWorld [
        "If this platform supports deferred updates, then make my canvas be the Display (or a rectangular portion of it), set the Display to deferred update mode, and answer true. Otherwise, do nothing and answer false. One can set the class variable DisableDeferredUpdates to true to completely disable the deferred updating feature."

	(Display deferUpdates: true) ifNil: [^ false].  "deferred updates not supported"
	(canvas notNil and: [canvas form == Display]) ifFalse: [
			aWorld viewBox: Display boundingBox.    "do first since it may clear canvas"
			self canvas: (Display getCanvas copyClipRect: Display boundingBox)].
	^ true

]

{ #category : #canvas }
WorldState >> doFullRepaint [

	damageRecorder doFullRepaint

]

{ #category : #'update cycle' }
WorldState >> doOneCycleFor: aWorld [
	"Do one cycle of the interaction loop. This method is called repeatedly when the world is running. This is a moderately private method; a better alternative is usually either to wait for events or to check the state of things from #step methods."

	self interCyclePause: MinCycleLapse.
	self doOneCycleNowFor: aWorld.
]

{ #category : #'update cycle' }
WorldState >> doOneCycleNowFor: aWorld [
	"Immediately do one cycle of the interaction loop.
	This should not be called directly, but only via doOneCycleFor:"

	DisplayScreen checkForNewScreenSize.

	"process user input events"
	LastCycleTime := Time millisecondClockValue.
	self handsDo: [:h |
		self activeHand: h.
		h processEvents.
		self activeHand: nil.
	].

	"the default is the primary hand"
	self activeHand: self hands first.

	aWorld runStepMethods.		"there are currently some variations here"
	self displayWorldSafely: aWorld.
]

{ #category : #'update cycle' }
WorldState >> doOneSubCycleFor: aWorld [
	"Like doOneCycle, but preserves activeHand."

	| currentHand |
	currentHand := self activeHand.
	self doOneCycleFor: aWorld.
	self activeHand: currentHand.
]

{ #category : #'update cycle' }
WorldState >> drawWorld: aWorld submorphs: submorphs invalidAreasOn: aCanvas [ 
	"Redraw the damaged areas of the given canvas and clear the damage list. Return a collection of the areas that
were redrawn."

	| rectList n morphs rects validList |
	rectList := damageRecorder invalidRectsFullBounds: aWorld viewBox.
	"sort by areas to draw largest portions first"
	rectList := rectList asArray sort: [:r1 :r2 | r1 area > r2 area].
	damageRecorder reset.
	n := submorphs size.
	morphs := OrderedCollection new: n * 2.
	rects := OrderedCollection new: n * 2.
	validList := OrderedCollection new: n * 2.
	rectList do: 
			[:dirtyRect | 
			dirtyRect allAreasOutsideList: validList
				do: 
					[:r | | mm rect i c remnantIntersects remnants rectToFill | 
					"Experimental top-down drawing --
			Traverses top to bottom, stopping if the entire area is filled.
			If only a single rectangle remains, then continue with the reduced rectangle."

					rectToFill := r.
					remnants := OrderedCollection with: r.
					i := 1.
					[remnants isEmpty or: [i > n]] whileFalse: 
							[mm := submorphs at: i.
							((remnantIntersects := remnants select: [:each | (mm fullBounds intersects: each)]) notEmpty and: [mm visible]) 
								ifTrue: 
									[morphs addLast: mm.
									
									rects addLast: (Rectangle merging: (remnantIntersects collect: [:each | mm fullBounds intersect: each])).
									remnants removeAll: remnantIntersects.
									remnantIntersects do: [:eachIntersect | remnants addAll: (mm areasRemainingToFill: eachIntersect)].
									remnants size = 1 ifTrue: [rectToFill := remnants first].
									remnants isEmpty ifTrue: [rectToFill := nil]].
							i := i + 1].
					"Now paint from bottom to top, but using the reduced rectangles."
					rectToFill 
						ifNotNil: [aWorld drawOn: (c := aCanvas copyClipRect: rectToFill)].
					[morphs isEmpty] whileFalse: 
							[(rect := rects removeLast) == rectToFill 
								ifFalse: [c := aCanvas copyClipRect: (rectToFill := rect)].
							c fullDrawMorph: morphs removeLast].
					morphs reset.
					rects reset.
					validList add: r]].
	^validList
]

{ #category : #'worldmenu building' }
WorldState >> fallbackMenuOn: menu [
	"Build the menu that is put up if something is going wrong with the menubuilder"
	
	menu addTitle: 'Fallback menu'.
	menu commandKeyHandler: self.
	menu addLine.
	menu
		defaultTarget: Smalltalk tools;
		addList: Smalltalk tools menuItems.
	menu addLine.
	menu add: 'Save'
		target: self class
		selector: #saveSession.
	menu add: 'Save as...'
		target: self class
		selector: #saveAs.
	menu add: 'Save and quit' 
		target: self class
		selector: #saveAndQuit.
	menu add: 'Quit' 
		target: self class
		selector: #quitSession.
]

{ #category : #'update cycle' }
WorldState >> forceDamageToScreen: allDamage [

	Display forceDamageToScreen: allDamage.
	
]

{ #category : #'update cycle' }
WorldState >> handleFatalDrawingError: errMsg [
	"Handle a fatal drawing error."
	Display deferUpdates: false. "Just in case"
	self primitiveError: errMsg.

	"Hm... we should jump into a 'safe' worldState here, but how do we find it?!"
]

{ #category : #hands }
WorldState >> hands [

	^ hands
]

{ #category : #hands }
WorldState >> handsDo: aBlock [

	^ hands do: aBlock
]

{ #category : #hands }
WorldState >> handsReverseDo: aBlock [

	^ hands reverseDo: aBlock
]

{ #category : #initialization }
WorldState >> initialize [

	super initialize.
	hands := Array new.
	damageRecorder:= DamageRecorder new.
	stepList := Heap sortBlock: self stepListSortBlock.
	lastStepTime := 0.
	lastAlarmTime := 0.
	self viewBox: Display boundingBox
]

{ #category : #'update cycle' }
WorldState >> interCyclePause: milliSecs [
	"delay enough that the previous cycle plus the amount of delay will equal milliSecs.  If the cycle is already expensive, then no delay occurs.  However, if the system is idly waiting for interaction from the user, the method will delay for a proportionally long time and cause the overall CPU usage of Squeak to be low.
	If self serverMode returns true then, always do a complete delay of 50ms, independant of my argument. This prevents the freezing problem described in Mantis #6581"

	| wait wait2 |
	wait := self serverMode
    	ifTrue: [ 50 ]
    	ifFalse: 
		[	wait2 := (lastCycleTime notNil and: [CanSurrenderToOS ~~ false]) 
        		ifFalse: [ 0 ]
        		ifTrue: [ lastCycleTime + milliSecs - Time millisecondClockValue ].
 
      		self flag: 'Issue 14754 - wait2>millisecs is only True for clock rollover. Remove it once DelayScheduler based on microsecondClock - Ben Coman 19.01.2015'.
      		wait2 > milliSecs 
        		ifTrue: [ 0 ]
        		ifFalse: [ wait2 ]. 
		].
		
	wait > 0 ifTrue: [	(Delay forMilliseconds: wait) wait ].

	lastCycleTime := Time millisecondClockValue.
	CanSurrenderToOS := true.
]

{ #category : #hands }
WorldState >> is: hand overlappingDamagedAreaIn: damageList [
	| handBounds |
	handBounds := hand fullBounds.
	^ damageList anySatisfy: [ :r | r intersects: handBounds ]
]

{ #category : #settings }
WorldState >> isEasySelecting [
	^ self class easySelectingWorld
]

{ #category : #stepping }
WorldState >> isStepping: aMorph [
	"Return true if the given morph is in the step list."
	lastStepMessage ifNotNil:[(lastStepMessage receiver == aMorph) ifTrue:[^true]].
	stepList do:[:entry| entry receiver == aMorph ifTrue:[^true]].
	^ false
]

{ #category : #stepping }
WorldState >> isStepping: aMorph selector: aSelector [
	"Return true if the given morph is in the step list."
	lastStepMessage ifNotNil:[
		(lastStepMessage receiver == aMorph and:[lastStepMessage selector == aSelector])
			ifTrue:[^true]].
	stepList do:[:entry| (entry receiver == aMorph and:[entry selector == aSelector]) ifTrue:[^true]].
	^ false
]

{ #category : #stepping }
WorldState >> listOfSteppingMorphs [
	^stepList collect:[:entry| entry receiver].

]

{ #category : #canvas }
WorldState >> recordDamagedRect: damageRect [

	damageRecorder ifNotNil: [damageRecorder recordInvalidRect: damageRect truncated]

]

{ #category : #alarms }
WorldState >> removeAlarm: aSelector for: aTarget [
	"Remove the alarm with the given selector"

	self alarms
		detect: [ :any | any receiver == aTarget and: [ any selector == aSelector ] ]
		ifFound: [ :alarm | self alarms remove: alarm ]
]

{ #category : #hands }
WorldState >> removeHand: aHandMorph [
	"Remove the given hand from the list of hands for this world."

	(hands includes: aHandMorph) ifFalse: [^self].
	hands := hands copyWithout: aHandMorph.
	activeHand == aHandMorph ifTrue: [activeHand := nil].
	ActiveHand == aHandMorph ifTrue: [ActiveHand := nil].

]

{ #category : #canvas }
WorldState >> resetDamageRecorder [

	damageRecorder reset

]

{ #category : #'worldmenu building' }
WorldState >> resetWorldMenu [
	menuBuilder 
		ifNotNil: [menuBuilder reset.
			menuBuilder := nil]
]

{ #category : #stepping }
WorldState >> runLocalStepMethodsIn: aWorld [ 
	"Run morph 'step' methods (LOCAL TO THIS WORLD) whose time has come. Purge any morphs that are no longer in this world.
	ar 3/13/1999: Remove buggy morphs from the step list so that they don't raise repeated errors."

	| now morphToStep stepTime priorWorld |
	now := Time millisecondClockValue.
	priorWorld := ActiveWorld.
	ActiveWorld := aWorld.
	self triggerAlarmsBefore: now.
	stepList isEmpty 
		ifTrue: 
			[ActiveWorld := priorWorld.
			^self].
	(now < lastStepTime or: [now - lastStepTime > 5000]) 
		ifTrue: [self adjustWakeupTimes: now].	"clock slipped"
	[stepList isEmpty not and: [stepList first scheduledTime < now]] 
		whileTrue: 
			[lastStepMessage := stepList removeFirst.
			morphToStep := lastStepMessage receiver.
			(morphToStep shouldGetStepsFrom: aWorld) 
				ifTrue: 
					[lastStepMessage value: now.
					lastStepMessage ifNotNil: 
							[stepTime := lastStepMessage stepTime ifNil: [morphToStep stepTime].
							lastStepMessage scheduledTime: now + (stepTime max: 1).
							stepList add: lastStepMessage]].
			lastStepMessage := nil].
	lastStepTime := now.
	ActiveWorld := priorWorld
]

{ #category : #stepping }
WorldState >> runStepMethodsIn: aWorld [
	"Perform periodic activity inbetween event cycles"
	
	| queue nextInQueue|
	"If available dispatch some deferred UI Message"
	queue := self class deferredUIMessages.
	[(nextInQueue := queue nextOrNil) isNil]
		whileFalse: [ nextInQueue value].
	self runLocalStepMethodsIn: aWorld.
	"The multi-threaded global Transcript needs to be updated periodically and synchronously with the UI."
	Transcript stepGlobal.
]

{ #category : #hands }
WorldState >> selectHandsToDrawForDamage: damageList [
	"Select the set of hands that must be redrawn because either (a) the hand itself has changed or (b) the hand intersects some damage rectangle."

	^ hands
		select: [ :hand | hand needsToBeDrawn and: [ hand hasChanged or: [ self is: hand overlappingDamagedAreaIn: damageList ] ] ]
]

{ #category : #settings }
WorldState >> serverMode [
	^ self class serverMode
]

{ #category : #initialization }
WorldState >> stepListSize [
	^ stepList size
]

{ #category : #stepping }
WorldState >> stepListSortBlock [
	^[ :stepMsg1 :stepMsg2 | 
		stepMsg1 scheduledTime <= stepMsg2 scheduledTime
	]
]

{ #category : #stepping }
WorldState >> stopStepping: aMorph [
	"Remove the given morph from the step list."
	lastStepMessage ifNotNil:[
		(lastStepMessage receiver == aMorph) ifTrue:[lastStepMessage := nil]].
	stepList removeAll: (stepList select:[:stepMsg| stepMsg receiver == aMorph]).

]

{ #category : #stepping }
WorldState >> stopStepping: aMorph selector: aSelector [
	"Remove the given morph from the step list."
	lastStepMessage ifNotNil:[
		(lastStepMessage receiver == aMorph and:[lastStepMessage selector == aSelector])
			ifTrue:[lastStepMessage := nil]].
	stepList removeAll: (stepList select:[:stepMsg| stepMsg receiver == aMorph and:[stepMsg selector == aSelector]]).
]

{ #category : #alarms }
WorldState >> triggerAlarmsBefore: nowTime [
	"Trigger all pending alarms that are to be executed before nowTime."
	| pending |
	lastAlarmTime ifNil:[lastAlarmTime := nowTime].
	(nowTime < lastAlarmTime or:[nowTime - lastAlarmTime > 10000])
		ifTrue:[self adjustAlarmTimes: nowTime].
	pending := self alarms.
	[pending isEmpty not and:[pending first scheduledTime < nowTime]]
		whileTrue:[pending removeFirst value: nowTime].
	lastAlarmTime := nowTime.
]

{ #category : #canvas }
WorldState >> viewBox [

	^ viewBox
]

{ #category : #canvas }
WorldState >> viewBox: x [

	viewBox := x
]

{ #category : #'worldmenu building' }
WorldState >> worldMenu [
	^self  menuBuilder menuEntitled: self discoveredMenuTitle.
]
