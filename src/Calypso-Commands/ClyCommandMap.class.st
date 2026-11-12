"
I am the commands Calypso shows for one anchor, by interaction (context menu, toolbar, tab actions, table icons, shortcuts, clicks, drag and drop and the left bar of text editors), and the browser tools opened as tabs (#tabs) and the decorators of table cells (#tableDecorators) for the same anchor.
An anchor is a class which says when the commands apply:
- a tool context class, such as ClyFullBrowserClassContext: the commands apply to the contexts which are instances of it or of its subclasses;
- an item type, such as ClyMethod or Package: the commands apply to the browser contexts whose selection holds items of this type.
I am built by running every class-side method of the anchor marked with the <calypsoCommands> pragma, in the order of their selectors. Each method receives me and adds its commands to my interactions. Any package can add such a method as an extension, which is how plugins add commands to Calypso:
	ClyMethod class >> myCoverageCommandsIn: aCommandMap
		<calypsoCommands>
		aCommandMap contextMenu add: MyShowCoverageCommand order: 1000.
		aCommandMap shortcuts add: MyShowCoverageCommand key: $k meta shift
I am plain data, built without any tool context. ClyCommandRegistry keeps me in a cache and finds the maps which apply to a context.
The guide doc/Calypso/Commands.md of the Pharo repository describes the anchors, the interactions and the menu groups.
"
Class {
	#name : 'ClyCommandMap',
	#superclass : 'Object',
	#instVars : [
		'anchor',
		'interactions',
		'declaringMethod'
	],
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'instance creation' }
ClyCommandMap class >> forAnchor: anAnchorClass [

	^ self new
		  setAnchor: anAnchorClass;
		  collectDeclarations;
		  yourself
]

{ #category : 'accessing' }
ClyCommandMap class >> pragmaName [
	"Class-side methods of an anchor marked with this pragma declare its commands. They take one argument, the command map. See the guide doc/Calypso/Commands.md of the Pharo repository."

	^ #calypsoCommands
]

{ #category : 'accessing' }
ClyCommandMap >> allEntries [

	^ interactions values flatCollect: [ :interaction | interaction entries ]
]

{ #category : 'accessing' }
ClyCommandMap >> anchor [

	^ anchor
]

{ #category : 'interactions' }
ClyCommandMap >> click [
	"Commands run by a click on an item of a list."

	^ self interactionNamed: #click entryClass: ClyMouseCommandEntry labelSelector: #name
]

{ #category : 'building' }
ClyCommandMap >> collectDeclarations [
	"Runs the declaration methods of my anchor in the order of their selectors, remembering the running one so each entry knows where it was declared. Then applies the overrides they recorded, so an override does not depend on the order of the methods."

	((Pragma allNamed: self class pragmaName in: anchor classSide) sorted: [ :first :second | first methodSelector <= second methodSelector ]) do: [ :pragma |
			declaringMethod := pragma method.
			[ anchor perform: pragma methodSelector with: self ] ensure: [ declaringMethod := nil ] ].
	interactions do: [ :interaction | interaction applyOverrides ]
]

{ #category : 'interactions' }
ClyCommandMap >> contextMenu [
	"The context menu of lists and text editors."

	^ self interactionNamed: #contextMenu entryClass: ClyCommandEntry labelSelector: #name
]

{ #category : 'accessing' }
ClyCommandMap >> declaringMethod [
	"The declaration method running while I am built."

	^ declaringMethod
]

{ #category : 'accessing' }
ClyCommandMap >> declaringMethod: aCompiledMethod [

	declaringMethod := aCompiledMethod
]

{ #category : 'interactions' }
ClyCommandMap >> doubleClick [
	"Commands run by a double click on an item of a list."

	^ self interactionNamed: #doubleClick entryClass: ClyMouseCommandEntry labelSelector: #name
]

{ #category : 'interactions' }
ClyCommandMap >> dragAndDrop [
	"Commands run when the selected items are dropped on another context, see ClyCommandInteraction>>#add:dropIn:."

	^ self interactionNamed: #dragAndDrop entryClass: ClyDropCommandEntry labelSelector: #name
]

{ #category : 'accessing' }
ClyCommandMap >> entriesOf: anInteractionName [

	^ (interactions at: anInteractionName ifAbsent: [ ^ #(  ) ]) entries
]

{ #category : 'initialization' }
ClyCommandMap >> initialize [

	super initialize.
	interactions := IdentityDictionary new
]

{ #category : 'private' }
ClyCommandMap >> interactionNamed: aSymbol entryClass: aCommandEntryClass labelSelector: aSelector [

	^ interactions at: aSymbol ifAbsentPut: [
			  ClyCommandInteraction
				  named: aSymbol
				  in: self
				  entryClass: aCommandEntryClass
				  labelSelector: aSelector ]
]

{ #category : 'printing' }
ClyCommandMap >> printOn: aStream [

	super printOn: aStream.
	aStream
		nextPut: $(;
		print: anchor;
		nextPut: $)
]

{ #category : 'initialization' }
ClyCommandMap >> setAnchor: anAnchorClass [

	anchor := anAnchorClass
]

{ #category : 'interactions' }
ClyCommandMap >> shortcuts [
	"Commands run by a key combination, see ClyCommandInteraction>>#add:key:."

	^ self interactionNamed: #shortcuts entryClass: ClyShortcutCommandEntry labelSelector: #name
]

{ #category : 'interactions' }
ClyCommandMap >> tabActions [
	"The action buttons of the browser tabs."

	^ self interactionNamed: #tabActions entryClass: ClyCommandEntry labelSelector: #name
]

{ #category : 'interactions' }
ClyCommandMap >> tableDecorators [
	"The decorators of the cells of the lists, subclasses of ClyTableDecorator."

	^ self interactionNamed: #tableDecorators entryClass: ClyTableDecoratorEntry labelSelector: #name
]

{ #category : 'interactions' }
ClyCommandMap >> tableIcons [
	"The icons shown in the cells of the lists."

	^ self interactionNamed: #tableIcons entryClass: ClyCommandEntry labelSelector: #name
]

{ #category : 'interactions' }
ClyCommandMap >> tabs [
	"The browser tools opened as tabs, subclasses of ClyBrowserToolMorph."

	^ self interactionNamed: #tabs entryClass: ClyTabEntry labelSelector: #name
]

{ #category : 'interactions' }
ClyCommandMap >> textLeftBarClick [
	"Commands run by a click in the left bar of a text editor."

	^ self interactionNamed: #textLeftBarClick entryClass: ClyMouseCommandEntry labelSelector: #name
]

{ #category : 'interactions' }
ClyCommandMap >> textLeftBarDoubleClick [
	"Commands run by a double click in the left bar of a text editor."

	^ self interactionNamed: #textLeftBarDoubleClick entryClass: ClyMouseCommandEntry labelSelector: #name
]

{ #category : 'interactions' }
ClyCommandMap >> textLeftBarMenu [
	"The menu of the left bar of a text editor."

	^ self interactionNamed: #textLeftBarMenu entryClass: ClyCommandEntry labelSelector: #name
]

{ #category : 'interactions' }
ClyCommandMap >> toolbar [
	"The toolbar of the browsers."

	^ self interactionNamed: #toolbar entryClass: ClyCommandEntry labelSelector: #defaultToolbarItemName
]
