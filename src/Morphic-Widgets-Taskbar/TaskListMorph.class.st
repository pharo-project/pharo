"
A themed task list/switcher to select the topmost window. Opens on Alt+Tab (Option+Tab on macOS).
Hold the modifier key to keep the switcher open; press Tab to cycle forward, Shift+Tab to cycle backward.
"
Class {
	#name : 'TaskListMorph',
	#superclass : 'Morph',
	#instVars : [
		'tasks'
	],
	#classVars : [
		'KeepOpen'
	],
	#category : 'Morphic-Widgets-Taskbar',
	#package : 'Morphic-Widgets-Taskbar'
}

{ #category : 'testing' }
TaskListMorph class >> isNavigationEvent: aKeyboardEvent [

	^ aKeyboardEvent asKeyCombination = Character tab alt or: [ aKeyboardEvent asKeyCombination = Character tab alt shift ]
]

{ #category : 'settings' }
TaskListMorph class >> keepOpen [
	^ KeepOpen ifNil: [KeepOpen := false]
]

{ #category : 'settings' }
TaskListMorph class >> keepOpen: aBoolean [
	KeepOpen := aBoolean
]

{ #category : 'settings' }
TaskListMorph class >> taskListSettingOn: aBuilder [
	<systemsettings>
	(aBuilder setting: #keepTaskListOpen)
		label: 'Keep task list open';
		description: 'Whether the tasklist is closed (and the selected window activated) when the command key is released';
		default: false;
		parent: #morphic;
		selector: #keepOpen;
		target: self
]

{ #category : 'private' }
TaskListMorph >> activeTask [

	"Answer the first active task"
	^ self tasks detect: #isActive ifNone: []
]

{ #category : 'private' }
TaskListMorph >> addMorphs [

	"Sub-morphs are populated by updateTaskButtons."
	self layoutChanged
]

{ #category : 'private' }
TaskListMorph >> baseBorderColor [

	^ self theme baseBorderColor
]

{ #category : 'private' }
TaskListMorph >> containerOverhead [

	"Each container adds ~16px overhead: max(borderWidth*2) + 2*containerLayoutInset"
	^ 16
]

{ #category : 'private' }
TaskListMorph >> containerPadding [

	^ 100
]

{ #category : 'private' }
TaskListMorph >> defaultPreviewExtent [

	"Answer the default extent for each individual task thumbnail."
	^ (140@100) scaledByDisplayScaleFactor
]

{ #category : 'open/close' }
TaskListMorph >> doOpenInWorld: aWorld [

	"Update the layout after opening."
	aWorld addMorphCentered: self
]

{ #category : 'running' }
TaskListMorph >> done [

	"Close the tasklist and make the active task current."
	self delete.
	self activeTask ifNotNil: [ :task | task activate ]
]

{ #category : 'private' }
TaskListMorph >> effectivePreviewExtent [

	"Answer the preview extent scaled down proportionally if there are too many
	tasks to fit within maxWidth, otherwise it uses the default.
	Accounts for the per-container overhead (borders + layout insets) of 16px."

	| maxWidth numberOfTasks scale |

	maxWidth := self maxWidth.
	numberOfTasks := self tasks size.
	numberOfTasks <= 1 ifTrue: [ ^ self defaultPreviewExtent ].

	scale := maxWidth - (self layoutInset * 2) - (numberOfTasks * self containerOverhead) - ((numberOfTasks - 1) * self cellInset)
	         / (numberOfTasks * self defaultPreviewExtent x).
	
	scale := (scale min: 1.0) max: 0.2.
	^ (self defaultPreviewExtent * scale) asIntegerPoint
]

{ #category : 'event handling' }
TaskListMorph >> handleEvent: event [

	"Handle Alt+Tab navigation directly to avoid wasHandled flag issues.
	When a SystemWindow delegates navigationKey: to the world, the event
	arrives here already marked as handled, which would skip handleKeyDown:.
	
	Only process keyDown events — keyUp also reaches here via
	sendFocusEvent:to:clear: and would produce a duplicate step."

	(self shouldHandleEvent: event) ifTrue: [ self moveTabSelection: event. ^ true ].
	
	^ super handleEvent: event
]

{ #category : 'event handling' }
TaskListMorph >> handlesKeyboard: event [

	^ true
]

{ #category : 'testing' }
TaskListMorph >> hasCurrentWindows: currentTasks [

	^ (self tasks includesAll: currentTasks) and: [ currentTasks includesAll: self tasks ]
]

{ #category : 'private' }
TaskListMorph >> highlightBorderColor [

	^ self theme selectedTabBorderColor
]

{ #category : 'initialization' }
TaskListMorph >> initialize [

	super initialize.
	self
		initializeTasks;
		initializeLayout;
		initializeAppearance;
		addMorphs;
		updateButtonsAndPreview
]

{ #category : 'initialization' }
TaskListMorph >> initializeAppearance [

	self
		color: (Color black alpha: 0.7);
		borderWidth: (self theme taskbarThumbnailBorderWidthFor: self);
		cornerStyle: (self theme taskbarThumbnailCornerStyleFor: self)
]

{ #category : 'initialization' }
TaskListMorph >> initializeLayout [

	self
		changeTableLayout;
		listDirection: #leftToRight;
		layoutInset: 16;
		cellInset: 12;
		cellPositioning: #center;
		vResizing: #shrinkWrap;
		hResizing: #shrinkWrap
]

{ #category : 'initialization' }
TaskListMorph >> initializeTasks [

	self tasks: self currentWorld taskbarTasks
]

{ #category : 'settings' }
TaskListMorph >> keepOpen [

	^ self class keepOpen
]

{ #category : 'event handling' }
TaskListMorph >> keyDown: event [


	"Handle Alt+Tab and Alt+Shift+Tab to cycle through tasks."

	(self class isNavigationEvent: event) ifTrue: [ self moveTabSelection: event. ^ true ].
	
	^ super keyDown: event
]

{ #category : 'private' }
TaskListMorph >> label: task truncation: labelTruncation size: size [

	^ StringMorph new
		  contents: (task label size > labelTruncation
				   ifTrue: [ (task label first: (labelTruncation - 4 max: 0)) , '...' ]
				   ifFalse: [ task label ]);
		  color: Color white;
		  fontName: 'Standard' size: size;
		  yourself
]

{ #category : 'private' }
TaskListMorph >> maxWidth [

	"Answer the maximum width the task list may occupy, derived from the world extent."
	
	^ self currentWorld width - self containerPadding
]

{ #category : 'geometry' }
TaskListMorph >> minimumExtent [

	"Answer the minimum extent."
	^ (self defaultPreviewExtent x + 32 @ (self defaultPreviewExtent y + 64))
]

{ #category : 'event handling' }
TaskListMorph >> moveTabSelection: event [

	event shiftPressed
		ifTrue: [ self selectPreviousTask ]
		ifFalse: [ self selectNextTask ]
]

{ #category : 'open/close' }
TaskListMorph >> openInWorld: aWorld [

	super openInWorld: aWorld.
	self center: aWorld center.
	self wantsKeyboardFocus ifTrue: [ self takeKeyboardFocus ]
]

{ #category : 'open/close' }
TaskListMorph >> postOpenInWorld: aWorld [

	"Update the layout after opening."
	self allMorphs do: #layoutChanged.
	super postOpenInWorld: aWorld
]

{ #category : 'open/close' }
TaskListMorph >> preOpenInWorld: aWorld [

	"Nothing to do"
]

{ #category : 'cycling' }
TaskListMorph >> selectNextTask [

	"Make the next task active."
	self selectTask: (self tasks after: self activeTask ifAbsent: [
		self tasks isEmpty ifFalse: [ self tasks first ]
	])
]

{ #category : 'cycling' }
TaskListMorph >> selectPreviousTask [
	"Make the previous task active."

	self selectTask: (self tasks before: self activeTask ifAbsent: [
		self tasks isEmpty ifFalse: [ self tasks last ]
	])
]

{ #category : 'private' }
TaskListMorph >> selectTask: givenTask [
	"Make the given task active and update the buttons."

	self tasks do: [:task | task state: #restored].
	givenTask ifNotNil: [ givenTask state: #active ].

	self updateButtonsAndPreview
]

{ #category : 'event handling' }
TaskListMorph >> shouldHandleEvent: event [

	^ event isKeyboard and: [ event isKeyDown and: [ self class isNavigationEvent: event] ]
]

{ #category : 'stepping' }
TaskListMorph >> step [
	"Check the sensor for the command key to see if we're done."

	(self keepOpen not and: [ self activeHand anyModifierKeyPressed not ]) ifTrue: [
		self done
	]
]

{ #category : 'stepping' }
TaskListMorph >> stepTime [

	^ 100
]

{ #category : 'open/close' }
TaskListMorph >> takesKeyboardFocus [
	"Answer whether the receiver can normally take keyboard focus."

	^ true
]

{ #category : 'private' }
TaskListMorph >> taskClicked: aTask [
	"A button for a task has been pressed.
	Close after selecting."

	self selectTask: aTask.
	self done
]

{ #category : 'private' }
TaskListMorph >> taskContainer: task [

	^ Morph new
		changeTableLayout;
		listDirection: #topToBottom;
		borderWidth: (task isActive ifTrue: [ 0 ] ifFalse: [ 2 ]);
		color: Color transparent;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		cellPositioning: #center;
		cellInset: 8;
		layoutInset: 4;
		yourself
]

{ #category : 'accessing' }
TaskListMorph >> tasks [

	^ tasks
]

{ #category : 'accessing' }
TaskListMorph >> tasks: someTasks [

	tasks := someTasks
]

{ #category : 'private' }
TaskListMorph >> thumbWrapper: task [

	^ Morph new
		changeTableLayout;
		color: Color transparent;
		borderWidth: (task isActive ifTrue: [ 4 ] ifFalse: [ 2 ]);
		borderColor: (task isActive
				 ifTrue: [ self highlightBorderColor ]
				 ifFalse: [ self baseBorderColor ]);
		cornerStyle: #rounded;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		yourself
]

{ #category : 'private' }
TaskListMorph >> uniformizeContainerWidths [
	"Make all direct children the same width (the widest), so labels are vertically aligned."

	| maxW |

	self submorphs ifEmpty: [ ^ self ].
	maxW := (self submorphs collect: [:tab | tab minExtent x]) max.
	self submorphs do: [:tab | tab width: maxW]
]

{ #category : 'private' }
TaskListMorph >> updateButtonsAndPreview [
	
	"Rebuild and recenter."

	self updateTaskButtons: self effectivePreviewExtent.
	self world ifNotNil: [ :window |
		self hResizing = #rigid ifFalse: [ self extent: self minExtent . self hResizing: #rigid; vResizing: #rigid ].
		self center: window center
	]
]

{ #category : 'private' }
TaskListMorph >> updateTask: task extent: previewExtent [

	| container thumbWrapper thumb label scaleFactor labelTruncation fontSize |
	
	scaleFactor := previewExtent x / self defaultPreviewExtent x.
	labelTruncation := (25 * scaleFactor) asInteger max: 8.
	fontSize := (11 * scaleFactor) asInteger max: 8.
	thumb := task morph taskThumbnailOfSize: previewExtent.
	thumb extent: previewExtent.

	thumbWrapper := (self thumbWrapper: task) addMorphBack: thumb; yourself.
	
	label := self label: task truncation: labelTruncation size: fontSize.
	container := (self taskContainer: task)
		addMorphBack: thumbWrapper;
		addMorphBack: label;
		on: #mouseUp send: #value to: [ self selectTask: task; done ];
		yourself.
	
	self addMorphBack: container
]

{ #category : 'private' }
TaskListMorph >> updateTaskButtons: previewExtent [

	"Create clickable thumbnail morphs for all tasks, arranged horizontally.
	macOS-style: white selection glow, dimmed inactive thumbnails,
	with the task name label below each thumbnail."

	self removeAllMorphs.
	self tasks do: [ :task | self updateTask: task extent: previewExtent ].
	self uniformizeContainerWidths
]
