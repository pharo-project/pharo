"
A VersionBrowser is a tool made for browsing the several versions of the provided method.
"
Class {
	#name : #VersionBrowser,
	#superclass : #Object,
	#instVars : [
		'rgMethod',
		'browser'
	],
	#category : #'Spec-Tools-VersionBrowser'
}

{ #category : #api }
VersionBrowser class >> browseVersionsForClass: aClass selector: aSelector [ 
	| method |
	method := (aClass compiledMethodAt: aSelector) ifNil: [ ^self ].
	
	^ self new
		browseVersionsOf: method
		
]

{ #category : #api }
VersionBrowser class >> browseVersionsForMethod: aRingMethod [
	^ self new
		browseVersionsOf: aRingMethod
		
]

{ #category : #'tool registry' }
VersionBrowser class >> registerToolsOn: registry [
	"Add ourselves to registry. See [Smalltalk tools]" 
	registry register: self as: #versionBrowser

]

{ #category : #menu }
VersionBrowser class >> versionsBrowserListMenu: aBuilder [
	<contextMenu>
	<versionBrowserListMenu>
	| browser model |
	model := aBuilder model.
	browser := aBuilder model browser.
	browser selectedMessage notNil
		ifTrue: [ 
			(aBuilder item: #'Compare to current')
				help: 'Compare selected version to the current version';
				target: model;
				selector: #compareToCurrentVersion.
			(aBuilder item: #'Compare to version...')
				help: 'Compare selected version to another selected version';
				target: model;
				selector: #compareToOtherVersion.
			(aBuilder item: #'Revert to selected version')
				help: 'Resubmit the selected version, so that it becomes the current version';
				action: [ model revert: browser selectedItem ]].
	(aBuilder item: #Senders)
		keyText: 'n';
		help: 'Browse all senders of this selector';
		action: [ model browseSenders ].
	(aBuilder item: #Implementors)
		keyText: 'm';
		help: 'Browse all implementors of this selector';
		action: [ model browseImplementors ];
		withSeparatorAfter 
]

{ #category : #menu }
VersionBrowser >> browseImplementors [
	SystemNavigation new browseAllImplementorsOf: rgMethod selector
]

{ #category : #menu }
VersionBrowser >> browseSenders [
	SystemNavigation new browseSendersOf: rgMethod selector name: 'Senders of ' , rgMethod selector autoSelect: rgMethod selector
]

{ #category : #'instance creation' }
VersionBrowser >> browseVersionsOf: aMethod [

	self setRGMethodFrom: aMethod.
	^ self buildBrowser
]

{ #category : #'instance creation' }
VersionBrowser >> browseVersionsOf: method class: aClass meta: meta category: msgCategory selector: aSelector [

	self setRGMethodFrom: method.
	^ self buildBrowser
]

{ #category : #accessing }
VersionBrowser >> browser [
	^ browser
]

{ #category : #'instance creation' }
VersionBrowser >> browserForList: aList [

	browser :=  MessageBrowser new.
	browser
		topologicSort: false;
		refreshingBlock: [:m | false];
		sortingBlock: [:a :b | (a annotationNamed:#versionIndex ifAbsent:[0]) < (b annotationNamed:#versionIndex ifAbsent:[0]) ];
		displayBlock: [:changeRecord | self displayStringsFor: changeRecord].
										
	browser toolbarModel
		emptyDropList;
		addItemLabeled: 'Side By Side' do: [ 
			self showDiffMorphWithConverter: (DiffMorphChangeRecordConverter 
				methodReference: browser textConverter method 
				referencesList: aList) ];
		addItemLabeled: 'Source' do: [ 
			self showSourceWithConverter: (SourceMethodConverter method: browser textConverter method) ];
		addItemLabeled: 'Diff' do: [ 
			self showSourceWithConverter: (DiffChangeRecordConverter 
				methodReference: browser textConverter method 
				referencesList: aList) ].
		
	browser toolbarModel versionModel 
		label: 'Revert';
		action: [ self revert: browser selectedItem ].
	browser listModel menu:[:menu | menu addAllFromPragma:'versionBrowserListMenu' target:self].
	^ browser.
]

{ #category : #'instance creation' }
VersionBrowser >> buildBrowser [
	| changeList b |

	changeList := self buildChangeList.
	
	self browserForList: changeList.
	
	b := browser
		messages: changeList;
		title: 'Recent versions of ' , rgMethod asString;
		openWithSpec.
	b takeKeyboardFocus.
		
	self showDiffMorphWithConverter: (DiffMorphChangeRecordConverter 
		methodReference: browser textConverter method 
		referencesList: changeList).
		
	^ b	
]

{ #category : #'instance creation' }
VersionBrowser >> buildChangeList [
	rgMethod sourcePointer ifNil:[ ^ #() ].
	
	^ (SourceFiles
		changeRecordsFrom: rgMethod sourcePointer
		className: rgMethod theNonMetaClassName
		isMeta: rgMethod isMetaSide)
		collectWithIndex: [ :c :i | | rg |
			rg := c asRingDefinition.
			rg annotationNamed: #versionIndex put: i ]
]

{ #category : #menu }
VersionBrowser >> compareTo:anOtherVersion withLabel:aLabel [
	| diff versions selected|
	selected := browser selectedMessage.
	versions := Array with:anOtherVersion with: (selected).
	diff := DiffPresenter new.
	diff	title: ('Comparison from {1} to {2}' format:{selected stamp . aLabel}).
	diff text:(DiffMorphChangeRecordConverter 
		methodReference: anOtherVersion 
		referencesList: versions) getText.
      diff openWithSpec.

]

{ #category : #menu }
VersionBrowser >> compareToCurrentVersion [
	self compareTo: rgMethod withLabel:'current version'
]

{ #category : #menu }
VersionBrowser >> compareToOtherVersion [
	| labels versions index selected |
	versions := self buildChangeList.
	labels := versions collect: [ :version | version stamp ].
	index := UIManager default chooseFrom: labels.
	index > 0
		ifFalse: [ ^ self ].
	selected := versions at: index.
	self compareTo: selected withLabel: selected stamp
]

{ #category : #accessing }
VersionBrowser >> displayStringsFor: aMethodVersion [
	| author version cleanStampParts |
	
	"a stamp is a string in the form  'GuillermoPolito 3/4/2015 14:33'"
	"It may be incomplete, empty or even start with spaces."
	cleanStampParts := aMethodVersion stamp trim substrings.
	
	author := cleanStampParts ifEmpty: [ '<missing>' ] ifNotEmpty: [ :parts | parts first ].
	version := cleanStampParts ifEmpty: [ '<missing>' ] ifNotEmpty: [ :parts | parts allButFirst joinUsing: ' ' ].
	
	^ {author.
	version.
	(aMethodVersion realClass name).
	(aMethodVersion selector).
	('{' , aMethodVersion category , '}')}
]

{ #category : #'instance creation' }
VersionBrowser >> revert: aChangeRecord [
	aChangeRecord 
		ifNil: [self inform: 'nothing selected, so nothing done']
		ifNotNil: [ rgMethod realClass compile: aChangeRecord sourceCode classified: aChangeRecord category withStamp: aChangeRecord stamp notifying:nil].
		
	"reinitialize me"	
	self setRGMethodFrom: (rgMethod realClass >> (rgMethod selector)).

	browser 
		messages: self buildChangeList;
		setSelectedIndex: 1.
]

{ #category : #private }
VersionBrowser >> setRGMethodFrom: aMethod [
"asHistorical, because active RGMethods don't always have the source pointer attached"
	rgMethod := aMethod asRingDefinition asHistorical.
]

{ #category : #'instance creation' }
VersionBrowser >> showDiffMorphWithConverter: aConverter [
	| behavior |
	
	behavior := browser textModel behavior.
	browser textModel: browser newDiff.
	browser needRebuild: false.
	browser openWithSpec.
	browser textConverter: aConverter.
	browser textModel behavior: behavior
]

{ #category : #'instance creation' }
VersionBrowser >> showSourceWithConverter: aConverter [
	| behavior |
	
	behavior := browser textModel behavior.
	browser textModel: browser newText.
	browser textModel beForCode.
	browser needRebuild: false.
	browser textModel behavior: behavior.
	browser textConverter: aConverter.

	browser openWithSpec.

]
