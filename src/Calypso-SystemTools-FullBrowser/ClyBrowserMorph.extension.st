Extension { #name : 'ClyBrowserMorph' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
ClyBrowserMorph >> browseClassNamed: aString [

	| classToBrowse |
	classToBrowse := (self system bindingOf: aString)
		ifNil: [ self chooseClassToBrowseFrom: aString ]
		ifNotNil: [:binding | classToBrowse := binding value].
	classToBrowse ifNil: [ ^self ].
	classToBrowse isClassOrTrait ifFalse: [ classToBrowse := classToBrowse class ].

	self spawnBrowser: ClyFullBrowserMorph withState: [ :browser |
		browser selectClass: classToBrowse]
]

{ #category : '*Calypso-SystemTools-FullBrowser' }
ClyBrowserMorph >> chooseClassToBrowseFrom: aClassNamePattern [
	| query result |
	query := (ClyAllClassesQuery sortedFrom: self systemScope)
		 filteredBy: (ClyItemNameFilter substringPattern: aClassNamePattern).
	result := query execute.
	result isEmpty ifTrue: [ ^nil ].
	result size = 1 ifTrue: [ ^result items first ].

	^[self searchDialog
		requestSingleObject: 'Choose a class to browse...'
		from: query] on: CmdCommandAborted, CmCommandAborted do: [ nil ]
]

{ #category : '*Calypso-SystemTools-FullBrowser' }
ClyBrowserMorph >> spawnFullBrowser [

	^self spawnBrowser: ClyFullBrowserMorph withState: [ :browser |
		self navigateSpawnedFullBrowser: browser]
]

{ #category : '*Calypso-SystemTools-FullBrowser' }
ClyBrowserMorph >> spawnHierarchyForClass: aClass selector: aSymbolOrNil [
	"For compatibility with SystemNavigation
	we should ensure current image environment if it is absent".
	navigationEnvironment ifNil: [
		self navigationEnvironment: ClyNavigationEnvironment currentImage.
		self setUpAvailablePlugins].
	self spawnBrowser: ClyFullBrowserMorph withState: [ :browser |
		browser selectClass: aClass.
		browser switchToFullClassHierarchy.
		aSymbolOrNil ifNotNil: [
			browser selectMethod: aClass >> aSymbolOrNil ]
	]
]
