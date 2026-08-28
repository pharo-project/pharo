"
Unit tests for QuickProjectLoaderPresenter main UI.
"
Class {
	#name : 'QuickProjectLoaderPresenterTest',
	#superclass : 'TestCase',
	#instVars : [
		'presenter',
		'repository',
		'tempFile'
	],
	#category : 'QuickProjectLoader-Tests',
	#package : 'QuickProjectLoader-Tests'
}

{ #category : 'running' }
QuickProjectLoaderPresenterTest >> setUp [

	super setUp.
	tempFile := FileSystem memory / 'test_presenter_repo.ston'.
	repository := QuickProjectRepository new.
	repository persistencePath: tempFile.

	repository addEntryNamed: 'Roassal' repository: 'github://ObjectProfile/Roassal/src' baseline: 'Roassal'.
	repository addEntryNamed: 'Seaside' repository: 'github://SeasideSt/Seaside:v3.4.6/repository' baseline: 'Seaside'.

	presenter := QuickProjectLoaderPresenter new.
	presenter repository: repository
]

{ #category : 'running' }
QuickProjectLoaderPresenterTest >> tearDown [

	presenter window ifNotNil: [ :w | w close ].
	QuickProjectLoaderPresenter closeWindow.
	QuickProjectRepository reset.
	tempFile deleteIfAbsent: [].
	super tearDown
]

{ #category : 'tests' }
QuickProjectLoaderPresenterTest >> testFilterProjects [

	presenter filterProjects: 'Seaside'.
	self assert: presenter projectList items size equals: 1.
	self assert: presenter projectList items first name equals: 'Seaside'.

	presenter filterProjects: ''.
	self assert: presenter projectList items size equals: 2
]

{ #category : 'tests' }
QuickProjectLoaderPresenterTest >> testKeymapRegistration [

	| builder |
	builder := KMKeymapBuilder new.
	QuickProjectLoaderPresenter quickLoadShortcutOn: builder.
	self assert: builder shortcut name equals: #toggleQuickProjectLoader
]

{ #category : 'tests' }
QuickProjectLoaderPresenterTest >> testPresenterInitializes [

	self assert: presenter projectList items size equals: 2.
	self deny: presenter removeButton isEnabled.
	self deny: presenter loadButton isEnabled
]

{ #category : 'tests' }
QuickProjectLoaderPresenterTest >> testPresenterShowsEntries [

	| items |
	items := presenter projectList items.
	self assert: items size equals: 2.
	self assert: (items anySatisfy: [ :e | e name = 'Roassal' ]).
	self assert: (items anySatisfy: [ :e | e name = 'Seaside' ])
]

{ #category : 'tests' }
QuickProjectLoaderPresenterTest >> testPresenterSortsEntries [

	| seaside |
	seaside := repository rawEntries detect: [ :e | e name = 'Seaside' ].
	repository recordLoadOf: seaside.
	presenter refreshList.

	self assert: presenter projectList items first name equals: 'Seaside'
]

{ #category : 'tests' }
QuickProjectLoaderPresenterTest >> testSelectionEnablesButtons [

	self deny: presenter removeButton isEnabled.
	self deny: presenter loadButton isEnabled.

	presenter projectList selectIndex: 1.

	self assert: presenter removeButton isEnabled.
	self assert: presenter loadButton isEnabled
]

{ #category : 'tests' }
QuickProjectLoaderPresenterTest >> testWorldMenuRegistration [

	| builder |
	builder := PragmaMenuBuilder pragmaKeyword: 'worldMenu' model: self.
	builder buildTree.
	self assert: (builder menuSpec itemWith: #QuickProjectLoader) notNil
]
