"
Themed synchronous taskbar (not using #step to poll windows). The buttons provide visual feedback as to whether a window is active or minimised (collapsed) plus a popup menu with options to restore, minimise, maximise and close the associated window. Optional (via settings) preview of the window while mouse is over a taskbar button.

"
Class {
	#name : #TaskbarMorph,
	#superclass : #Morph,
	#instVars : [
		'tasks',
		'orderedTasks'
	],
	#classVars : [
		'MaximumButtons',
		'ShowTaskbar',
		'ShowWindowPreview'
	],
	#category : #'Morphic-Widgets-Taskbar'
}

{ #category : #initializing }
TaskbarMorph class >> initialize [
	"Initialize the receiver."
	self showTaskbarPreferenceChanged
]

{ #category : #setting }
TaskbarMorph class >> maximumButtons [
	^ MaximumButtons ifNil: [MaximumButtons := self maximumButtonsDefault]
]

{ #category : #setting }
TaskbarMorph class >> maximumButtons: anInteger [
	(anInteger isKindOf: Integer) ifFalse: [ ^ self ].
	anInteger > 0 ifFalse: [ ^ self ].
	MaximumButtons := anInteger.
]

{ #category : #setting }
TaskbarMorph class >> maximumButtonsDefault [
	^100
]

{ #category : #initialization }
TaskbarMorph class >> reset [
	<script>
	"Remove the taskbar and add a new one." 
	
	(self currentWorld submorphs select: [:m | m isKindOf: self])
		do: [:tb | tb delete].
	self currentWorld createTaskbarIfNecessary
]

{ #category : #setting }
TaskbarMorph class >> showTaskbar [
	^ ShowTaskbar ifNil: [ShowTaskbar := true]
]

{ #category : #setting }
TaskbarMorph class >> showTaskbar: aBoolean [
	ShowTaskbar ~= aBoolean
		ifTrue: [ShowTaskbar := aBoolean.
			self showTaskbarPreferenceChanged]
]

{ #category : #setting }
TaskbarMorph class >> showTaskbarPreferenceChanged [
	"Add or remove the taskbar as appropriate.
	Delegate to the current World."
	
	self currentWorld showWorldTaskbar:  self showTaskbar
]

{ #category : #setting }
TaskbarMorph class >> showWindowPreview [
	^ ShowWindowPreview ifNil: [ShowWindowPreview := true]
]

{ #category : #setting }
TaskbarMorph class >> showWindowPreview: aBoolean [
	ShowWindowPreview := aBoolean
]

{ #category : #setting }
TaskbarMorph class >> taskbarSettingOn: aBuilder [
	<systemsettings>
	(aBuilder setting: #showTaskbar)
		target: self;
		parent: #morphic;
		label: 'Taskbar';
		default: true;
		description: 'Whether the world''s taskbar should be shown or not.';
		with: [
			(aBuilder setting: #showWindowPreview)
				label: 'Window preview' ;
				default: true;
				description: 'Whether the world''s taskbar buttons should show previews of the associated window while the mouse is over them.'.
			(aBuilder setting: #maximumButtons)
				label: 'Maximum number of tasks to show';
				default: 100;
				description: ('By default, the value is ', self maximumButtonsDefault printString).

		]
]

{ #category : #taskbar }
TaskbarMorph >> buttonForMorph: aMorph [
	"Answer the button corresonding to the given
	morph or nil if none."
	
	|index|
	index := (self orderedTasks collect: [:t | t morph]) indexOf: aMorph.
	^index = 0 ifTrue: [nil] ifFalse: [self submorphs at: index ifAbsent: []]
]

{ #category : #drawing }
TaskbarMorph >> clipSubmorphs [ 

	^ true
]

{ #category : #'change reporting' }
TaskbarMorph >> displayExtentChanged [

	self updateBounds
]

{ #category : #'private - accessing' }
TaskbarMorph >> edgeToAdhereTo [
	"Must implement. Answer #bottom."
	
	^#bottom
]

{ #category : #'event handling' }
TaskbarMorph >> handlesMouseDown: evt [
	"Best to say we will to avoid being grabbed."
	
	^true
]

{ #category : #taskbar }
TaskbarMorph >> indicateModalChildForMorph: aMorph [
	"Flash the button corresonding to the given morph ."
	
	(self buttonForMorph: aMorph) ifNotNil: [:b |
		b indicateModalChild]
]

{ #category : #initialization }
TaskbarMorph >> initialize [
	"Initialize the receiver."

	super initialize.
	self
		initializeLayout;
		initializeAppearance;
		tasks: #();
		orderedTasks: OrderedCollection new
]

{ #category : #initialization }
TaskbarMorph >> initializeAppearance [
	"Initialize the appearance."

	self
		color: (self theme textColor alpha: 0.3);
		fillStyle: (self theme taskbarFillStyleFor: self)
]

{ #category : #initialization }
TaskbarMorph >> initializeLayout [
	"Initialize the layout."

	self
		changeTableLayout;
		layoutInset: 2;
		cellInset: 2;
		listDirection: #leftToRight;
		wrapDirection: #topToBottom;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		extent: self minimumExtent
]

{ #category : #initialization }
TaskbarMorph >> intoWorld: aWorld [
	"Stick to the bottom left now."
	
	self
		setToAdhereToEdge: #bottomLeft;
		updateBounds.
	super intoWorld: aWorld
]

{ #category : #testing }
TaskbarMorph >> isAdheringToBottom [
	"Must implement. Answer true."
	
	^true
]

{ #category : #testing }
TaskbarMorph >> isAdheringToLeft [
	"Must implement. Answer false."
	
	^false
]

{ #category : #testing }
TaskbarMorph >> isAdheringToRight [
	"Must implement. Answer false."
	
	^false
]

{ #category : #testing }
TaskbarMorph >> isAdheringToTop [
	"Must implement. Answer false."
	
	^false
]

{ #category : #testing }
TaskbarMorph >> isDockingBar [
	"Answer yes so we get updated when the Display is resized."

	^true
]

{ #category : #testing }
TaskbarMorph >> isTaskbar [
	"Answer true."

	^true
]

{ #category : #geometry }
TaskbarMorph >> minimumExtent [
	"Answer the minimum extent."

	^40@25
]

{ #category : #'wiw support' }
TaskbarMorph >> morphicLayerNumber [
	"Helpful for ensuring some morphs always appear in front of or 
	behind others. Smaller numbers are in front"
	
	^11
]

{ #category : #accessing }
TaskbarMorph >> orderedTasks [
	"Answer the value of orderedTasks"

	^ orderedTasks
]

{ #category : #accessing }
TaskbarMorph >> orderedTasks: anObject [
	"Set the value of orderedTasks"

	orderedTasks := anObject
]

{ #category : #'change reporting' }
TaskbarMorph >> ownerChanged [
	"The receiver's owner has changed its layout.
	Since this method is called synchronously in the
	ui, delete the receiver if there are any excpetions."
	
	self owner ifNil: [^self].
	[self updateBounds.
	self updateTasks]
		on: Exception
		do: [:ex | self delete. ex pass].
	super ownerChanged
]

{ #category : #theme }
TaskbarMorph >> preferredButtonCornerStyle [
	"Answer the preferred button corner style
	for submorphs."

	^#square
]

{ #category : #recategorized }
TaskbarMorph >> rejectsEvent: anEvent [
	(anEvent isMouse and: [ anEvent isMouseDown ]) ifTrue: [ ^ (self submorphs anySatisfy: [ :each | each containsPoint: anEvent cursorPoint ]) not ].
	
	^ super rejectsEvent: anEvent
]

{ #category : #taskbar }
TaskbarMorph >> removeFromWorld [
	"Delete the receiver from its world after restoring minimized tasks.
	Collapse those that were minimized after removal.
	Turn window animation off for the duration."

	|mins animation|
	mins := self tasks select: [:t | t isMinimized].
	animation := self theme shouldUseAnimations.
	animation ifTrue: [self theme useAnimation: false].
	[mins do: [:t | t morph restore; resetCollapsedFrame].
	self delete.
	mins do: [:t | t morph minimize]]
		ensure: [animation ifTrue: [self theme useAnimation: true]]
]

{ #category : #setting }
TaskbarMorph >> showWindowPreview [
	^ self class showWindowPreview
]

{ #category : #taskbar }
TaskbarMorph >> taskButtonOf: aMorph [
	"Answer the task button of the given morph or nil if none."

	^self submorphs detect: [:t | t model = aMorph] ifNone: []
]

{ #category : #taskbar }
TaskbarMorph >> taskOf: aMorph [
	"Answer the task of the given morph or nil if none."

	^self orderedTasks detect: [:t | t morph = aMorph] ifNone: []
]

{ #category : #accessing }
TaskbarMorph >> tasks [
	"Answer the value of tasks"

	^ tasks
]

{ #category : #accessing }
TaskbarMorph >> tasks: anObject [
	"Set the value of tasks"

	tasks := anObject
]

{ #category : #theme }
TaskbarMorph >> themeChanged [
	"The theme has changed. Update our appearance."

	self initializeAppearance.
	self removeAllMorphs.
	super themeChanged.
	self updateTaskButtons
]

{ #category : #'private - layout' }
TaskbarMorph >> updateBounds [
	"Update the receiver's bounds to fill the world."

	self
		width: self owner width;
		snapToEdgeIfAppropriate
]

{ #category : #private }
TaskbarMorph >> updateOrderedTasksFrom: tasksThatShouldBeUpdated [
	| deadTasks |
	deadTasks := OrderedCollection new.
	self orderedTasks
		do: [ :aTaskbarTask | 
			tasksThatShouldBeUpdated
				detect:
					[ :aTaskThatShouldBeUpdated | aTaskThatShouldBeUpdated morph = aTaskbarTask morph ]
				ifFound: [ :foundTask | tasksThatShouldBeUpdated remove: foundTask ]
				ifNone: [ deadTasks add: aTaskbarTask ] ].
	(deadTasks isEmpty and: [ tasksThatShouldBeUpdated isEmpty ])
		ifTrue: [ ^ self ].
	self orderedTasks: (self orderedTasks 
		removeAll: deadTasks;
		addAll: tasksThatShouldBeUpdated;
		yourself)
]

{ #category : #private }
TaskbarMorph >> updateTaskButtons [
	"Make buttons for the ordered tasks."
	
	| oldButtons size |
	oldButtons := self submorphs copy.
	self removeAllMorphs.
	self defer: [oldButtons do: [:b | b model: nil]]. "release dependency after event handling"
	
	size := self orderedTasks size.
	(self orderedTasks copyFrom: ( size - self class maximumButtons + 1 max: 1 ) to: size) do: [:t | | button |
		button := t taskbarButtonFor: self.
		button ifNotNil: [self addMorphBack: button]]
]

{ #category : #taskbar }
TaskbarMorph >> updateTasks [
	"Check for changes in the world's submorphs.
	Note that if the task attributes change then a
	task will be considered dead along with a new replacement."

	| wm deadTasks newTasks taskbarTasksFromWorldMorph |
	wm := self worldMorphs asOrderedCollection.
	taskbarTasksFromWorldMorph := (wm collect: [ :m | m taskbarTask ])
		reject: [ :m | m isNil ].
	self tasks: taskbarTasksFromWorldMorph.
	deadTasks := self orderedTasks difference: self tasks.
	newTasks := self tasks difference: self orderedTasks.
	(newTasks isEmpty and: [ deadTasks isEmpty ])
		ifTrue: [ ^ self ].	"no changes"
	newTasks copy
		do: [ :t | 
			self orderedTasks
				detect: [ :ot | ot morph = t morph ]
				ifFound: [ :ot | 
					self orderedTasks replaceAll: ot with: t.
					deadTasks remove: ot.
					newTasks remove: t ] ].	"replace in order any changed tasks."
	self orderedTasks
		removeAll: deadTasks;
		addAll: newTasks reversed.
	self updateTaskButtons.
	self
		defer: [ "may have a different number of rows" self layoutChanged ]
]

{ #category : #accessing }
TaskbarMorph >> wantsToBeTopmost [
	"Answer if the receiver want to be one of the topmost
	objects in its owner."
	
	^ true
]

{ #category : #taskbar }
TaskbarMorph >> worldMorphs [
	"Answer the world's submorphs plus those in hand.
	Nasty case since hand removes the morph before dropping"
	
	^self world submorphs, 
		((self tasks
			select: [:t | t morph owner = self world activeHand])
			collect: [:t | t morph])
]
