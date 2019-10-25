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

{ #category : #initialization }
DebuggerListPresenter >> getDebuggerClasses [
	^ (((PragmaCollector filter: [:prg | prg selector = #'iAmADebugger' ]) reset collected) collect: [ :debuggerMethod | debuggerMethod methodClass soleInstance ]) sort: [:dbgClass1 :dbgClass2 | dbgClass1 rank <= dbgClass2 rank]; yourself.
]

{ #category : #initialization }
DebuggerListPresenter >> iconManager [
	^ Smalltalk ui icons.
]

{ #category : #initialization }
DebuggerListPresenter >> initializeWidgets [
	debuggerList := self newTable
		addColumn: (SpStringTableColumn title: 'Rank' evaluated: #rank);
		addColumn: (SpStringTableColumn title: 'Debugger' evaluated: #yourself);
		items: self getDebuggerClasses.
	actionBar := self newToolBar
		addItem: (SpToolBarButton new
			label: 'Browse';
			icon: (self iconManager iconNamed: #smallFind);
			action: [ debuggerList selectedItems ifNotEmpty: [:debuggerList_ | (debuggerList_ at: 1) browse]]).
]
