Class {
	#name : #DebuggerListPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'debuggerList',
		'actionBar'
	],
	#category : #DebuggerSelector
}

{ #category : #specs }
DebuggerListPresenter class >> defaultSpec [

	^ SpBoxLayout newVertical
		add: #debuggerList;
		add: #actionBar.
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
	^ ((PragmaCollector filter: [:prg | prg selector = #'debugger' ]) reset collected) collect: [ :debuggerMethod | debuggerMethod methodClass soleInstance ].
]

{ #category : #initialization }
DebuggerListPresenter >> initializeWidgets [
	debuggerList := SpTablePresenter new
		addColumn: (SpStringTableColumn title: 'Rank' evaluated: #rank);
		addColumn: (SpStringTableColumn title: 'Debugger' evaluated: #yourself);
		items: self getDebuggerClasses.
	actionBar := SpActionBarPresenter new.
]
