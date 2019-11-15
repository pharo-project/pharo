"
Lists all the debuggers available in the image, along with their rank (provided they always return the same rank regardless of the debug session). Also list all the debug sessions stored in DebugSessionStorage.
"
Class {
	#name : #DebuggerListAndDebugSessionListPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'debuggerList',
		'debuggerListToolBar',
		'debugSessionList',
		'debugSessionListToolBar',
		'storageLimitLabel',
		'storageLimitInputField',
		'shouldUpdateStorageLimitInputField'
	],
	#category : #'Tools-Debugger'
}

{ #category : #specs }
DebuggerListAndDebugSessionListPresenter class >> defaultSpec [
	| toolBarHeight titleHeight |
	titleHeight := 20.
	toolBarHeight := 35.
	^ SpPanedLayout newHorizontal
		add: (SpBoxLayout newVertical
			add: 'Debuggers' height: titleHeight;
			add: #debuggerList;
			add: #debuggerListToolBar withConstraints: [:cstr | cstr height: toolBarHeight];
			yourself);
		add: (SpBoxLayout newVertical
			add: (SpBoxLayout newHorizontal
				add: 'Debug Sessions' width: 200;
				add: (SpBoxLayout newHorizontal
					add: #storageLimitLabel width: 80;
					add: #storageLimitInputField width: 70;
					yourself);
				yourself)
				height: titleHeight;
			add: #debugSessionList;
			add: #debugSessionListToolBar withConstraints: [:cstr | cstr height: toolBarHeight];
			yourself		
		);
		yourself
]

{ #category : #api }
DebuggerListAndDebugSessionListPresenter class >> initialExtent [
	^ 750@300
]

{ #category : #'world menu' }
DebuggerListAndDebugSessionListPresenter class >> menuCommandOn: aBuilder [
	<worldMenu>
	(aBuilder item: #'List Debuggers and Debug Sessions')
		parent: #Debugging;
		order: 3;
		help: 'Shows the list all available debuggers in the image, and all debug sessions';
		action: [ self open ]
]

{ #category : #api }
DebuggerListAndDebugSessionListPresenter class >> open [
	<example>
	| windowPresenter presenter |
	presenter := self new.
	windowPresenter := presenter openWithSpec.
	windowPresenter whenClosedDo: presenter getWhenClosedBlock.
	windowPresenter window extent: self initialExtent.
	^ windowPresenter
]

{ #category : #'world menu' }
DebuggerListAndDebugSessionListPresenter class >> title [
	^ 'Debuggers and Debug Sessions'
]

{ #category : #initialization }
DebuggerListAndDebugSessionListPresenter >> addCorrectMainDebuggerButtonToToolBar: aToolBar [
	debuggerList selectedItems ifNotEmpty: [ :debuggerList_ | (DebuggerSelector mainDebuggerClass == (debuggerList_ at: 1)) ifTrue: [ 
			"A debugger is selected in the list, and it is the main debugger class, so we add a button allowing the user to remove this debugger from the main debugger class"
			aToolBar addItem: (SpToolBarButton new
			label: 'Main Debugger';
			icon: (self iconManager iconNamed: #checkboxSelected);
			action: [ DebuggerSelector mainDebuggerClass: nil. self refreshDebuggerList. self refreshDebuggerListToolBar ]
		).
		^ self
	 ]].

	"Otherwise (no debugger is selected, or a debugger is selected and it is not the main debugger class), we add a buttont allowing to set the selected debugger as main debugger"
	aToolBar addItem: (SpToolBarButton new
			label: 'Main Debugger';
			icon: (self iconManager iconNamed: #checkboxUnselected);
			action: [
				debuggerList selectedItems ifEmpty: [
					self inform: 'You need to select a debugger'
				] ifNotEmpty: [:debuggerList_ |
					DebuggerSelector mainDebuggerClass: (debuggerList_ at: 1) ].
					self refreshDebuggerList.
					self refreshDebuggerListToolBar.
			]
		).
]

{ #category : #helpers }
DebuggerListAndDebugSessionListPresenter >> compareRank: rank1 to: rank2 [
	rank1 isInteger ifFalse: [ ^ false ]. "If rank1 is not an integer, (typically a string), then it is smaller than rank2"
	rank2 isInteger ifFalse: [ ^ true ]. "If rank2 is not an integer, (typically a string), then it is smaller than rank1"
	^ rank1 >= rank2 "Otherwise, both ranks are int, we return the result of their comparison"
]

{ #category : #'button action' }
DebuggerListAndDebugSessionListPresenter >> debugDebugSessionAction [
	"Describe the action performed by the 'Debug' button under the Debug Session List.
	Requires a debug session to be selected in the list of debug sessions, and a debugger to be selected in the list of debuggers. If these conditions are not met, inform the user and stop.
	If they are met, tell the selected debugger to open the selected debug session"
	
	| debugSession debuggerClass |
	debugSession := (debugSessionList selectedItems ifEmpty: [ self inform: 'You must selected a Debug Session to debug'. ^ self ] ifNotEmpty: [:selectedItems | selectedItems at: 1]) debugSession.
	debuggerClass := debuggerList selectedItems ifEmpty: [ self inform: 'You must select a debugger'. ^ self ] ifNotEmpty: [:selectedItems | selectedItems at: 1].
	debuggerClass rankDebugSession: debugSession. "Give debuggerClass a chance to signal the DebugSessionCannotBeOpenedByThisDebugger exception if it cannot open debugSession"
	debuggerClass openOn: debugSession withFullView: true andNotification: nil
]

{ #category : #initialization }
DebuggerListAndDebugSessionListPresenter >> debuggerClassToNameOfDebugger: aDebuggerClass [
	"Returns the name of @aDebuggerClass, to be displayed in the list of debuggers. If @aDebuggerClass is the main debugger defined in DebuggerSelector, return the name as a Text styled in bold"
	(aDebuggerClass == DebuggerSelector mainDebuggerClass ) ifTrue: [ 
		^ aDebuggerClass asString asText allBold; yourself
	] ifFalse: [ 
		^ aDebuggerClass asString
	]
]

{ #category : #helpers }
DebuggerListAndDebugSessionListPresenter >> getBlockTryingToGetRankOfDebugger [
	"Returns a block taking a debugger as argument and returning the rank it would give to a nil debug session. If an exception is raised, the debugger really needs the debug session object to give a rank, so the block returns a string saying that. If no exception is raised, it means the debugger did not need an actual debug session object to answer, the block directly return the returned value"
	^ [ :aDebugger | [aDebugger rankDebugSession: nil] on: Exception do: [ 'Depends on the debug session'] ]
]

{ #category : #helpers }
DebuggerListAndDebugSessionListPresenter >> getSortingBlock [
	| rankBlock |
	rankBlock := self getBlockTryingToGetRankOfDebugger.
	^ [ :aDebuggerClass1 :aDebuggerClass2 |
			|rank1 rank2|
			rank1 := rankBlock value: aDebuggerClass1.
			rank2 := rankBlock value: aDebuggerClass2.
			self compareRank: rank1 to: rank2.
		]
]

{ #category : #helpers }
DebuggerListAndDebugSessionListPresenter >> getWhenClosedBlock [
	"Returns the block to be executed when the windows containing this presenter is closed by the user"
	^ [ self setInitialSubscriptions: false ]
]

{ #category : #icon }
DebuggerListAndDebugSessionListPresenter >> iconManager [
	^ Smalltalk ui icons.
]

{ #category : #initialization }
DebuggerListAndDebugSessionListPresenter >> initializeWidgets [
	| timeOfStorageColumnWidth refsColumnWidth |
	debuggerList := self newTable
		addColumn: (SpStringTableColumn title: 'Rank' evaluated: self getBlockTryingToGetRankOfDebugger);
		addColumn: (SpStringTableColumn title: 'Debugger' evaluated: [:debuggerClass | self debuggerClassToNameOfDebugger: debuggerClass]);
		whenSelectionChangedDo: [self refreshDebuggerListToolBar].
		"Items are added to this list by #refreshDebuggerList"
	self refreshDebuggerList.
	debuggerListToolBar := self newToolBar.
	self refreshDebuggerListToolBar.
			
	timeOfStorageColumnWidth := 115.
	refsColumnWidth := 40.
	debugSessionList := self newTable
		addColumn: ((SpStringTableColumn title: 'Refs' evaluated: [:aDebugSessionStorageUnit | aDebugSessionStorageUnit debugSession countObjectsThatWantToKeepMeAlive asString]) width: refsColumnWidth; yourself);
		addColumn: (SpStringTableColumn title: 'Name' evaluated: [:aDebugSessionStorageUnit | aDebugSessionStorageUnit debugSession name] );
		addColumn: ((SpStringTableColumn title: 'Time of Storage' evaluated: [:aDebugSessionStorageUnit | self printDateAndTime: aDebugSessionStorageUnit timeOfCreation]) width: timeOfStorageColumnWidth; yourself ).
	self refreshDebugSessionList.
	
	debugSessionListToolBar := self makeDebugSessionListToolbar.
			
	storageLimitLabel := self newLabel
		label: 'storage limit:';
		help: 'Determines the maximum amount of debug sessions to be stored. Oldest debug sessions are terminated first. Debug sessions currently opened in any debugger are never terminated and do not count towards this limit.';
		yourself.
	storageLimitInputField := SpNumberInputFieldPresenter new
		numberType: Integer;
		minimum: 1;
		number: DebugSessionStorage storageLimitForDebugSessionsNotKeptAlive;
		whenNumberChangedDo: [ :newNumber | "Disable the automatic refreshing of this value in the UI during the update of the value, to avoid infinite loop" self shouldUpdateStorageLimitInputField: false. DebugSessionStorage storageLimitForDebugSessionsNotKeptAlive: newNumber. self shouldUpdateStorageLimitInputField: true ].
			
	self setInitialSubscriptions: true
]

{ #category : #helpers }
DebuggerListAndDebugSessionListPresenter >> makeDebugSessionListToolbar [
	| toolBar |
	toolBar := self newToolBar.
	toolBar addItem: (SpToolBarButton new
		label: 'Debug';
		icon: (self iconManager iconNamed: #glamorousBug);
		action: [ self debugDebugSessionAction ]
	).
	toolBar addItem: (SpToolBarButton new
			label: 'Inspect';
			icon: (self iconManager iconNamed: #smallFind);
			action: [debugSessionList selectedItems ifNotEmpty: [:selectedItems | (selectedItems at: 1) inspect]]).
	toolBar addItem: (SpToolBarButton new
		label: 'Terminate';
		icon: (self iconManager iconNamed: #remotesManagerRemoveRemote);
		action: [ debugSessionList selectedItems ifNotEmpty: [:selectedItems | DebugSessionStorage terminate: (selectedItems at: 1). self refreshDebugSessionList ] ]
	).
	toolBar addItem: (SpToolBarButton new
			label: 'Terminate All';
			icon: (self iconManager iconNamed: #glamorousTrash);
			action: [DebugSessionStorage terminateAll. self refreshDebugSessionList]).
	toolBar addItem: (SpToolBarButton new
			label: 'Clean Up';
			icon: (self iconManager iconNamed: #warning); "Should be a broom icon instead, but I did not find any"
			action: [ DebugSessionStorage enforceStorageLimit]).
	^ toolBar
]

{ #category : #'react to events' }
DebuggerListAndDebugSessionListPresenter >> onDebugSessionStorageLimitUpdate [
	self shouldUpdateStorageLimitInputField ifTrue: [
		self shouldUpdateStorageLimitInputField: false. "Deactivate the auto-refreshing while updating the value, to avoid infinite loop"
		storageLimitInputField number: DebugSessionStorage storageLimitForDebugSessionsNotKeptAlive.
		self shouldUpdateStorageLimitInputField: true.
	]
]

{ #category : #'react to events' }
DebuggerListAndDebugSessionListPresenter >> onDebugSessionStorageUpdate [
	self refreshDebugSessionList
]

{ #category : #'react to events' }
DebuggerListAndDebugSessionListPresenter >> onMainDebuggerClassUpdate [
	self refreshDebuggerList.
	self refreshDebuggerListToolBar.
]

{ #category : #initialization }
DebuggerListAndDebugSessionListPresenter >> printDateAndTime: aDateAndTime [
	| d |
	d := aDateAndTime.
	^ ' ', d hour asString, ':', d minute asString, ' ', d year asString, '-', d month asString, '-', d dayOfMonth asString.
]

{ #category : #helpers }
DebuggerListAndDebugSessionListPresenter >> refreshDebugSessionList [
	debugSessionList items: DebugSessionStorage storage
]

{ #category : #initialization }
DebuggerListAndDebugSessionListPresenter >> refreshDebuggerList [
	debuggerList items: (DebuggerSelector getDebuggerClasses sort: self getSortingBlock)
]

{ #category : #initialization }
DebuggerListAndDebugSessionListPresenter >> refreshDebuggerListToolBar [
	debuggerListToolBar removeAllItems.
	debuggerListToolBar addItem: (SpToolBarButton new
			label: 'Browse';
			icon: (self iconManager iconNamed: #smallFind);
			action: [ debuggerList selectedItems ifNotEmpty: [:debuggerList_ | (debuggerList_ at: 1) browse]]).
	self addCorrectMainDebuggerButtonToToolBar: debuggerListToolBar.
]

{ #category : #announcement }
DebuggerListAndDebugSessionListPresenter >> setInitialSubscriptions: aBoolean [
	"If @aBoolean is true, perform the initial subscriptions needed to keep the UI up to date. If it is false, unsubscribe. The point of having both the subscriptions and unsubscriptions in the same method is not to forget to add unsubscription statement in the future when new subscription are added"
	aBoolean ifTrue: [ 
		DebugSessionStorage addSubscriberOnStorageUpdate: self.
		DebugSessionStorage addSubscriberOnStorageLimitUpdate: self.
		DebuggerSelector addSubscribersOnMainDebuggerClassUpdate: self
	] ifFalse: [ 
		DebugSessionStorage removeSubscriberOnStorageUpdate: self.
		DebugSessionStorage removeSubscriberOnStorageLimitUpdate: self.
		DebuggerSelector removeSubscribersOnMainDebuggerClassUpdate: self
	]
]

{ #category : #accessing }
DebuggerListAndDebugSessionListPresenter >> shouldUpdateStorageLimitInputField [
	shouldUpdateStorageLimitInputField ifNil: [ shouldUpdateStorageLimitInputField := true ].
	^ shouldUpdateStorageLimitInputField
]

{ #category : #accessing }
DebuggerListAndDebugSessionListPresenter >> shouldUpdateStorageLimitInputField: aBoolean [
	shouldUpdateStorageLimitInputField := aBoolean
]
