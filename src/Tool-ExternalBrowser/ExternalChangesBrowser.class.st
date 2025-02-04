"
I'm a simple changes browser. When the user drop a .cs or .st files and select to view them, I display a sequenceable and simple list of changes and a way to file in the code. 

!! Examples

[[[
ExternalChangesBrowser new openWithSpec.

ExternalChangesBrowser openOn: Smalltalk changesFile fullName
]]]
"
Class {
	#name : 'ExternalChangesBrowser',
	#superclass : 'SpPresenter',
	#instVars : [
		'changes',
		'codePane',
		'changeSet',
		'selectAllButton',
		'deselectAllButton',
		'fileInSelectedButton'
	],
	#category : 'Tool-ExternalBrowser-Browsers',
	#package : 'Tool-ExternalBrowser',
	#tag : 'Browsers'
}

{ #category : 'examples' }
ExternalChangesBrowser class >> browseRecentLog [
	<example>

	^ self openOnStream: SourceFiles changesFileStream
]

{ #category : 'layout' }
ExternalChangesBrowser class >> defaultLayout [
	<spec: #default>

	^ SpPanedLayout newTopToBottom
		add: (SpBoxLayout newTopToBottom
					add: #changes;
					add: (SpBoxLayout newLeftToRight
							add: #selectAllButton;
							add: #deselectAllButton;
							add: #fileInSelectedButton;
							yourself ) height: self buttonHeight );
		add: #codePane;
		yourself
]

{ #category : 'accessing' }
ExternalChangesBrowser class >> defaultPreferredExtent [

	^ 500@600
]

{ #category : 'examples' }
ExternalChangesBrowser class >> example1 [

	<example>
	self new open
]

{ #category : 'examples' }
ExternalChangesBrowser class >> example2 [
	<example>

	self openOn: Smalltalk changesFile fullName
]

{ #category : 'file service' }
ExternalChangesBrowser class >> fileReaderServicesForFile: fullName suffix: suffix [
	<fileService>

	| services |
	services := OrderedCollection new.

	(ExternalBrowser sourceFileSuffixes includes: suffix) |

	('changes' = suffix) | (suffix = '*')
		ifTrue: [ services add: self serviceBrowseCSOrSTFile ].
	^services
]

{ #category : 'instance creation' }
ExternalChangesBrowser class >> openOn: changeSetName [

	self new
		changeSetName: changeSetName;
		open
]

{ #category : 'instance creation' }
ExternalChangesBrowser class >> openOnFileName: aFileName [

	self new
		changeSetName: aFileName;
		open
]

{ #category : 'instance creation' }
ExternalChangesBrowser class >> openOnStream: aStream [

	| widget |
	widget := self new.
	widget changes items: (CodeImporter chunksFromStream: aStream).
	widget open
]

{ #category : 'tool registry' }
ExternalChangesBrowser class >> registerToolsOn: registry [

	registry register: self as: #changeList
]

{ #category : 'file service' }
ExternalChangesBrowser class >> serviceBrowseCSOrSTFile [
	"Answer a service for opening a changelist browser on a file"

	^ (SimpleServiceEntry
		provider: self
		label: 'Changelist browser'
		selector: #openOnStream:
		description: 'Open a changelist tool on this file'
		buttonLabel: 'Changes')
		argumentGetter: [ :stream | stream readStream ]
]

{ #category : 'file service' }
ExternalChangesBrowser class >> services [
	"Answer potential file services associated with this class"

	^ { self serviceBrowseCSOrSTFile }
]

{ #category : 'icons' }
ExternalChangesBrowser class >> taskbarIconName [
	"Answer the icon for the receiver in a task bar."

	^#smallSave
]

{ #category : 'menu' }
ExternalChangesBrowser >> buildMenu [

	| aMenu |

	aMenu := self newMenu.

	aMenu 	addItem: [ :item |
			item
				name: 'File in all selected';
				action: [ self fileIn: changes selectedItemsSorted ] ].

	aMenu 	addItem: [ :item |
			item
				name:  'Compare method to current';
				enabled: [ self canCompareToCurrent ];
				action: [ self compareToCurrent: changes selectedItem ] ].

	^ aMenu
]

{ #category : 'menu' }
ExternalChangesBrowser >> canCompareToCurrent [

	| aMethodDeclaration |
	(changes selectedItems size = 1) ifFalse: [ ^ false ].
	((aMethodDeclaration := changes selectedItem) isMethodDeclaration) ifFalse: [  ^ false ].

	( aMethodDeclaration existsBehavior) ifFalse: [ ^ false ].
	^ aMethodDeclaration targetClass includesSelector: aMethodDeclaration methodSelector
]

{ #category : 'initialization' }
ExternalChangesBrowser >> changeDescription: change [

	^ change description
]

{ #category : 'initialization' }
ExternalChangesBrowser >> changeSetName: aFileName [

	changeSet := CodeImporter fileNamed: aFileName.
	changeSet parseChunks.
	changes items: changeSet codeDeclarations
]

{ #category : 'accessing' }
ExternalChangesBrowser >> changes [

	^ changes
]

{ #category : 'accessing' }
ExternalChangesBrowser >> codePane [

	^ codePane
]

{ #category : 'menu' }
ExternalChangesBrowser >> compareToCurrent: aMethodDeclaration [

	| class sourceString |
	class := aMethodDeclaration targetClass.
	sourceString := (class >> aMethodDeclaration methodSelector) sourceCode.

	self
		openPolymorphComparisonFrom: aMethodDeclaration contents
		to: sourceString
		belongingTo: class
		from: aMethodDeclaration
		labeled: 'Current'
		inWindowLabeled: 'Comparison to Current Version'
]

{ #category : 'initialization' }
ExternalChangesBrowser >> connectPresenters [

	changes display: [ :elem | self changeDescription: elem ].

	changes whenSelectionChangedDo: [ :selection | selection selectedItem ifNotNil: [ codePane text: selection selectedItem contents ] ].

	selectAllButton action: [ changes selectAll ].
	deselectAllButton action: [ changes resetListSelection ].
	fileInSelectedButton action: [ self fileIn: changes selectedItemsSorted ].

	changes contextMenu: self buildMenu
]

{ #category : 'accessing' }
ExternalChangesBrowser >> deselectAllButton [

	^ deselectAllButton
]

{ #category : 'menu' }
ExternalChangesBrowser >> fileIn: aCodeDeclaration [

	aCodeDeclaration do: #import
]

{ #category : 'accessing' }
ExternalChangesBrowser >> fileInSelectedButton [

	^ fileInSelectedButton
]

{ #category : 'initialization' }
ExternalChangesBrowser >> initializePresenters [


	changes := self newList.
	changes beMultipleSelection.

	selectAllButton := self newButton.
	selectAllButton
		label: 'select all'.

	deselectAllButton := self newButton.
	deselectAllButton
		label: 'deselect all'.

	fileInSelectedButton := self newButton.
	fileInSelectedButton
		label: 'file in selected'.

	codePane := self newCode
]

{ #category : 'private' }
ExternalChangesBrowser >> openPolymorphComparisonFrom: targetMethodSource to: originalMethodSource belongingTo: aClass from: aChange labeled: aLabel inWindowLabeled: aWindowLabel [

	| diffMorph |
	diffMorph := DiffChangeMorph
		             from: targetMethodSource
		             label: aChange stamp
		             to: originalMethodSource
		             label: (aClass compiledMethodAt: aChange methodSelector) timeStamp
		             contextClass: aClass.

	diffMorph openInWindowLabeled: aWindowLabel
]

{ #category : 'accessing' }
ExternalChangesBrowser >> selectAllButton [

	^ selectAllButton
]

{ #category : 'initialization' }
ExternalChangesBrowser >> windowTitle [

	^ 'Changes Browser'
]
