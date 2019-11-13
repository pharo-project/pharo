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
		'debugSessionListToolBar'
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
			add: 'Debug Sessions' height: titleHeight;
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
	^ [ DebugSessionStorage removeSubscriberOnStorageUpdate: self ]
]

{ #category : #icon }
DebuggerListAndDebugSessionListPresenter >> iconManager [
	^ Smalltalk ui icons.
]

{ #category : #initialization }
DebuggerListAndDebugSessionListPresenter >> initializeWidgets [
	| timeOfStorageColumnWidth |
	timeOfStorageColumnWidth := 115.
	debugSessionList := self newTable
		addColumn: (SpStringTableColumn title: 'Name' evaluated: [:aDebugSessionStorageUnit | aDebugSessionStorageUnit debugSession name] );
		addColumn: ((SpStringTableColumn title: 'Time of Storage' evaluated: [:aDebugSessionStorageUnit | aDebugSessionStorageUnit timeOfCreation]) width: timeOfStorageColumnWidth; yourself ).
	self refreshDebugSessionList.
	
	debugSessionListToolBar := self makeDebugSessionListToolbar.

	debuggerList := self newTable
		addColumn: (SpStringTableColumn title: 'Rank' evaluated: self getBlockTryingToGetRankOfDebugger);
		addColumn: (SpStringTableColumn title: 'Debugger' evaluated: #yourself);
		items: (Debugger getDebuggerClasses sort: self getSortingBlock).
	debuggerListToolBar := self newToolBar
		addItem: (SpToolBarButton new
			label: 'Browse';
			icon: (self iconManager iconNamed: #smallFind);
			action: [ debuggerList selectedItems ifNotEmpty: [:debuggerList_ | (debuggerList_ at: 1) browse]]).
			
	DebugSessionStorage addSubscriberOnStorageUpdate: self.
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
	^ toolBar
]

{ #category : #initialization }
DebuggerListAndDebugSessionListPresenter >> onDebugSessionStorageUpdate [
	self refreshDebugSessionList
]

{ #category : #helpers }
DebuggerListAndDebugSessionListPresenter >> refreshDebugSessionList [
	debugSessionList items: DebugSessionStorage storage
]
