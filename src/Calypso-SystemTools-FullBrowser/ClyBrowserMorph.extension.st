Extension { #name : 'ClyBrowserMorph' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
ClyBrowserMorph >> browseClass: aClass [

	| classToBrowse |
	aClass ifNil: [ ^ self ].

	classToBrowse := aClass isClassOrTrait
		                 ifFalse: [ aClass class ]
		                 ifTrue: [ aClass ].

	self spawnBrowser: ClyFullBrowserMorph withState: [ :browser | browser selectClass: classToBrowse ]
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
