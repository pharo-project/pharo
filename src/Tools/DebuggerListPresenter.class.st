"
Lists all the debuggers available in the image, along with their rank (provided they always return the same rank regardless of the debug session)
"
Class {
	#name : #DebuggerListPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'debuggerList',
		'actionBar'
	],
	#category : #'Tools-Debugger'
}

{ #category : #specs }
DebuggerListPresenter class >> defaultSpec [
	| toolbarHeight |
	toolbarHeight := 35.
	^ SpBoxLayout newVertical
		add: #debuggerList;
		add: #actionBar withConstraints: [:cstr | cstr height: toolbarHeight];
		yourself.
]

{ #category : #'world menu' }
DebuggerListPresenter class >> menuCommandOn: aBuilder [
	<worldMenu>
	(aBuilder item: #'List Debuggers')
		parent: #Debugging;
		order: 3;
		help: 'Lists all available debuggers in the image.';
		action: [ self new openWithSpec ]
]

{ #category : #'world menu' }
DebuggerListPresenter class >> title [
	^ 'List of Available Debuggers'
]

{ #category : #helpers }
DebuggerListPresenter >> compareRank: rank1 to: rank2 [
	rank1 isInteger ifFalse: [ ^ false ]. "If rank1 is not an integer, (typically a string), then it is smaller than rank2"
	rank2 isInteger ifFalse: [ ^ true ]. "If rank2 is not an integer, (typically a string), then it is smaller than rank1"
	^ rank1 >= rank2 "Otherwise, both ranks are int, we return the result of their comparison"
]

{ #category : #helpers }
DebuggerListPresenter >> getBlockTryingToGetRankOfDebugger [
	"Returns a block taking a debugger as argument and returning the rank it would give to a nil debug session. If an exception is raised, the debugger really needs the debug session object to give a rank, so the block returns a string saying that. If no exception is raised, it means the debugger did not need an actual debug session object to answer, the block directly return the returned value"
	^ [ :aDebugger | [aDebugger rankDebugSession: nil] on: Exception do: [ 'Depends on the debug session'] ]
]

{ #category : #helpers }
DebuggerListPresenter >> getSortingBlock [
	| rankBlock |
	rankBlock := self getBlockTryingToGetRankOfDebugger.
	^ [ :aDebuggerClass1 :aDebuggerClass2 |
			|rank1 rank2|
			rank1 := rankBlock value: aDebuggerClass1.
			rank2 := rankBlock value: aDebuggerClass2.
			self compareRank: rank1 to: rank2.
		]
]

{ #category : #icon }
DebuggerListPresenter >> iconManager [
	^ Smalltalk ui icons.
]

{ #category : #initialization }
DebuggerListPresenter >> initializeWidgets [
	debuggerList := self newTable
		addColumn: (SpStringTableColumn title: 'Rank' evaluated: self getBlockTryingToGetRankOfDebugger);
		addColumn: (SpStringTableColumn title: 'Debugger' evaluated: #yourself);
		items: (Debugger getDebuggerClasses sort: self getSortingBlock).
	actionBar := self newToolBar
		addItem: (SpToolBarButton new
			label: 'Browse';
			icon: (self iconManager iconNamed: #smallFind);
			action: [ debuggerList selectedItems ifNotEmpty: [:debuggerList_ | (debuggerList_ at: 1) browse]]).
]
