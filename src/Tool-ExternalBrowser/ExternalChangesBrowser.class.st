"
I'm a simple changes browser. When the user drop a .cs or .st files and select to view them, I display a sequenceable and simple list of changes and a way to file in the code. 

!! Examples

[[[
ExternalChangesBrowser new openWithSpec.

ExternalChangesBrowser openOn: Smalltalk changesFile fullName
]]]
"
Class {
	#name : #ExternalChangesBrowser,
	#superclass : #ComposablePresenter,
	#instVars : [
		'changes',
		'codePane',
		'changeSet',
		'selectAllButton',
		'deselectAllButton',
		'fileInSelectedButton'
	],
	#category : #'Tool-ExternalBrowser-Browsers'
}

{ #category : #examples }
ExternalChangesBrowser class >> browseRecentLog [
	<example>
	
	^ self openOnStream: SourceFiles changesFileStream
]

{ #category : #specs }
ExternalChangesBrowser class >> defaultSpec [
	<spec: #default>
	
	^ SpecLayout composed 
		newColumn: [ :col | 
			col
				add: #changes;
				newRow: [ :row | 
					row
						add: #selectAllButton;
						add: #deselectAllButton;
						add: #fileInSelectedButton ] height: 32;
				add: #codePane ]; 
		yourself
]

{ #category : #examples }
ExternalChangesBrowser class >> example1 [
	<example>
	
	self new openWithSpec

]

{ #category : #examples }
ExternalChangesBrowser class >> example2 [
	<example>
	
	self openOn: Smalltalk changesFile fullName
]

{ #category : #'file service' }
ExternalChangesBrowser class >> fileReaderServicesForFile: fullName suffix: suffix [
	<fileService>
	| services |
	services := OrderedCollection new.
	(FileStream isSourceFileSuffix: suffix) | 
	(FileStream isChangesFileSuffix: suffix) | (suffix = '*')
		ifTrue: [ services add: self serviceBrowseCSOrSTFile ].
	^services
]

{ #category : #'instance creation' }
ExternalChangesBrowser class >> openOn: changeSetName [

	self new
		changeSetName: changeSetName;
		openWithSpec
]

{ #category : #'instance creation' }
ExternalChangesBrowser class >> openOnFileName: aFileName [

	self new
		changeSetName: aFileName;
		openWithSpec
]

{ #category : #'instance creation' }
ExternalChangesBrowser class >> openOnStream: aStream [

	| widget |
	widget := self new.
	widget changes items: (CodeImporter chunksFromStream: aStream).
	widget openWithSpec	
]

{ #category : #'tool registry' }
ExternalChangesBrowser class >> registerToolsOn: registry [

	registry register: self as: #changeList 
]

{ #category : #'file service' }
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

{ #category : #'file service' }
ExternalChangesBrowser class >> services [
	"Answer potential file services associated with this class"

	^ { self serviceBrowseCSOrSTFile }
]

{ #category : #icons }
ExternalChangesBrowser class >> taskbarIconName [
	"Answer the icon for the receiver in a task bar."

	^#smallSaveIcon
]

{ #category : #menu }
ExternalChangesBrowser >> buildMenu: aMenu [

	aMenu
		add: 'File in all selected'
		target: self
		selector: #fileIn:
		argumentList: { changes selectedItems }.
	
	aMenu
		addToggle: 'Compare method to current'
		target: self
		selector: #compareToCurrent:
		getStateSelector:nil
		enablementSelector: #canCompareToCurrent
		argumentList: { changes selectedItem }.

	^ aMenu
]

{ #category : #menu }
ExternalChangesBrowser >> canCompareToCurrent [

	| aMethodDeclaration |
	(changes selectedItems size = 1) ifFalse: [ ^ false ].
	((aMethodDeclaration := changes selectedItem) isMethodDeclaration) ifFalse: [  ^ false ].
	
	( aMethodDeclaration existsBehavior) ifFalse: [ ^ false ].
	^ aMethodDeclaration targetClass includesSelector: aMethodDeclaration methodSelector
	
]

{ #category : #initialization }
ExternalChangesBrowser >> changeDescription: change [

	^ change description
]

{ #category : #initialization }
ExternalChangesBrowser >> changeSetName: aFileName [

	changeSet := CodeImporter fileNamed: aFileName.
	changeSet parseChunks.
	changes items: changeSet codeDeclarations.
]

{ #category : #accessing }
ExternalChangesBrowser >> changes [

	^ changes
]

{ #category : #accessing }
ExternalChangesBrowser >> codePane [

	^ codePane
]

{ #category : #menu }
ExternalChangesBrowser >> compareToCurrent: aMethodDeclaration [

	| class sourceString |
	class := aMethodDeclaration targetClass.
	sourceString := (class >> aMethodDeclaration methodSelector) sourceCode.
	UIManager default
		openComparisonFrom: aMethodDeclaration contents
		to: sourceString
		belongingTo: class
		from: aMethodDeclaration
		labeled: 'Current'
		inWindowLabeled: 'Comparison to Current Version'
]

{ #category : #accessing }
ExternalChangesBrowser >> deselectAllButton [

	^ deselectAllButton
]

{ #category : #menu }
ExternalChangesBrowser >> fileIn: aCodeDeclaration [

	aCodeDeclaration do: #import.
]

{ #category : #accessing }
ExternalChangesBrowser >> fileInSelectedButton [

	^ fileInSelectedButton
]

{ #category : #initialization }
ExternalChangesBrowser >> initialExtent [

	^ 500@600
]

{ #category : #initialization }
ExternalChangesBrowser >> initializePresenter [
	changes whenSelectedItemChanged: [ :change | change ifNotNil: [ codePane text: change contents ] ].
	changes menu: [ :menu :shifted | self buildMenu: menu ].
	selectAllButton action: [ changes selectAll ].
	deselectAllButton action: [ changes resetSelection ].
	fileInSelectedButton action: [ self fileIn: changes selectedItemsSorted ]
]

{ #category : #initialization }
ExternalChangesBrowser >> initializeWidgets [

	self title: 'Changes Browser'.

	changes := self newList.
	changes displayBlock: [ :elem | self changeDescription: elem ].
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

	codePane := self newCode.
]

{ #category : #accessing }
ExternalChangesBrowser >> selectAllButton [

	^ selectAllButton
]
