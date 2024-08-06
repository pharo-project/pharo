"
I am a base class for various browser implementations.
My main subcasses are ClyFullBrowser (for an advanced 4 panes browser) and ClyQueryBrowser (to display results of senders/implementors/...).

I provide UI layout structure for my subclasses:

- all navigation views occupy the top half of the browser.
- the tabs panel is placed at the bottom half of the browser.
- and the toolbar is placed at the middle.

Navigation is represented by ClyQueryView(Morph) instances placed from left to right. This reflects the flow of navigation: selection in the left panel leads to the new content at the right panel.

!! Example of Queries 

I create query instances using current state like selections, metalevel scope (class/inst side) and current queries themselves. 
In some cases it is quite complex logic and it requires interaction between different objects. For exampe the construction logic of methodGroupQuery is very complex:
	- the different query class for variable mode (vars in protocols pane)
	- in variable mode the scope is different (vars are shown from all superclasses)
	- extra query composition when query is build from scope of extended classes (grey classes are selected)
	- extra logic to allow default traits visibility
	- some other details.
(it could be extracted to new kind of queries and scopes)

!! Browser Logic 
Another part of browser logic is defined in methods like #selectMethod:, #selectClass:, selectPackage:. A browser knows that to select method it should first select its class. To select class it should first select its package. In some cases it is also not trivial logic. Look at selectClass: method. 

Ideally browser should be a model itself independently from UI.  But this point deserves another iteration. The main concern of the current version was to introduce queries to manage browser state. It simplifies a lot of behaviour but it still not enough to get really clean solution. 


!! Browser contexts 

Browser contexts are not for maintaining the state. They only represent possible state of components. They are approach to have pluggability points for commands, tabs, toolbars and table decorators. Remember that commands are annotated with activators for particular context where they should be used. Exactly the same logic is used for other parts of browser. Everything you read in Commander chapter is applicable for tabs, table decorators and toolbar items. Tabs are annotated with ClyTabActivationStrategy. Table decorators are annotated with ClyTableDecorationStrategy.

So a browser collects contexts from children because otherwise children will need to know about toolbar and tabs. Now they only know the browser. Also all contexts are used to build spotter command menu (cmd+/). Query views has no information about it.  


!! Browser changes 

I implement logic how and when rebuild tabs and toolbar. Any browser change should be wrapped by method #changeStateBy:

	browser changeStateBy: [ packageView selection selectItemsWith: { 'Kernel' asPackage } ]
	
Any selection change can lead to the changes in all related navigation views which follow navigation flow. I ensure in this method that tabs and toolbar will be rebuilt only when navigation will be completely finished. However this is only when all views will set new content and selection that I will update tabs and toolbar.

Also I manage navigation history by allowing go back and forward in the browser. And this method also ensures that intermediate navigation states will not be considered as navigation. Many selection changes can be triggered from single #changeStateBy: call. But I will add only one item to the history.

I provide two methods to force go back and forward navigation: 

	browser navigateBack.
	browser navigateForward.

For more details on history implementation look at ClyNavigationHistory.





!! How to create new browsers


Subclasses should implement #initializeNavigationViews to configure the number of navigation panes and their properties.
They should create navigation views using #newNavigationView message: 

	packageView := self newNavigationView.
	
The content of view should be set in another methods (see bellow). During initialization you should only configure structure of the view.
For example by default created view will show single column with the name of item.
But you can configure different label using following method: 

	packageView mainColumn 
		displayItemPropertyBy: [:packageItem | packageItem name, packageItem actualObject classes size asString].

(the argument of the block is instance of ClyDataSourceItem which wrap actual object retrieved by query).

Also to describe navigation flow you should setup selector which should called when user will select any item: 
	
	packageView requestNavigationBy: #packageSelectionChanged.
	 
Look at ClyQueryView(Morph) to find more possible settings and browser senders of #newNavigationView (for example you can add more columns to the view).

To setup the content of the navigation views you should implement method #prepareInitialState. For the package view example it can be: 

	packages := ClyAllPackages sortedFrom: self systemScope.
	packageView showQuery: packages 

You do not need to set up the content of all navigation views. They have kind of empty data source by default.
During navigation you will configure them in the navigation request methods. You will create appropriate queries for them based on new selected objects.

The last responsibility of subclasses is to implement #newWindowTitle. It is used to setup the title of window which contains the browser. And it is updated when state of browser is changed.









Internal Representation and Key Implementation Points.

    Instance Variables
	navigationEnvironment:		<ClyNavigationEnvironment>
	navigationHistory:		<ClyNavigationHistory>
	navigationPanel:		<Morph>
	navigationStarted:		<Boolean>
	navigationViews:		<OrderedCollection of<ClyQueryView>>
	plugins:		<Collection of<ClyBrowserPlugin>>
	systemScope:		<ClySystemScope>
	tabManager:		<ClyTabManager>
	toolPanel:		<Morph>
	toolbar:		<ClyToolbar>
"
Class {
	#name : 'ClyBrowserMorph',
	#superclass : 'PanelMorph',
	#instVars : [
		'navigationPanel',
		'navigationViews',
		'toolPanel',
		'toolbar',
		'tabManager',
		'navigationHistory',
		'navigationStarted',
		'plugins',
		'navigationEnvironment',
		'systemScope'
	],
	#classVars : [
		'NavigationHistoryClass'
	],
	#category : 'Calypso-Browser-UI',
	#package : 'Calypso-Browser',
	#tag : 'UI'
}

{ #category : 'tools registration' }
ClyBrowserMorph class >> beAllDefault [
	<script>
	self subclasses select: [ :each | each canBeDefault  ] thenDo: [ :each | each beDefaultBrowser ]
]

{ #category : 'testing' }
ClyBrowserMorph class >> canBeDefault [
	^self class includesSelector: #beDefaultBrowser
]

{ #category : 'settings' }
ClyBrowserMorph class >> navigationHistoryClass [

	^ NavigationHistoryClass
		ifNil: [ NavigationHistoryClass := ClyNavigationHistory ]
]

{ #category : 'settings' }
ClyBrowserMorph class >> navigationHistoryClass: aClyNavigationHistory [

	NavigationHistoryClass := aClyNavigationHistory
]

{ #category : 'instance creation' }
ClyBrowserMorph class >> on: aNavigationEnvironment [
	^self new
		navigationEnvironment: aNavigationEnvironment;
		setUpAvailablePlugins
]

{ #category : 'instance creation' }
ClyBrowserMorph class >> on: aNavigationEnvironment systemScope: aSystemScope [
	^(self on: aNavigationEnvironment)
		systemScope: aSystemScope
]

{ #category : 'settings' }
ClyBrowserMorph class >> settingsNavigationHistoryOn: aBuilder [
	<systemsettings>

	(aBuilder pickOne: #navigationHistoryClass)
		parent: #Calypso;
		label: 'Navigation History';
		domainValues: ClyNavigationHistory withAllSubclasses;
		default: ClyNavigationHistory;
		description: 'Enable to set a custom navigation history to handle how collection of visited items is managed';
		target: self
]

{ #category : 'accessing' }
ClyBrowserMorph >> activeStatusBar [

	^tabManager activeStatusBar
]

{ #category : 'accessing' }
ClyBrowserMorph >> addPlugin: aBrowserPlugin [
	plugins detect: [ :each | each class = aBrowserPlugin class ] ifFound: [ ^self ].

	aBrowserPlugin browser: self.
	plugins add: aBrowserPlugin
]

{ #category : 'accessing' }
ClyBrowserMorph >> allContexts [

	| result |
	result := OrderedCollection new.

	self allContextsDo: [:each | result add: each ].

	^result
]

{ #category : 'accessing' }
ClyBrowserMorph >> allContextsDo: aBlock [

	self navigationContextsDo: aBlock.

	tabManager tools
		select: [ :each | each isKindOf: ClyTextEditorToolMorph ]
		thenDo: [ :each | aBlock value: each createTextContext ]
]

{ #category : 'navigation' }
ClyBrowserMorph >> allNavigationScopes [

	| defaultScope |
	defaultScope := self defaultNavigationScope.

	^self systemScope = defaultScope
		ifTrue: [ {self systemScope} ]
		ifFalse: [ { self systemScope. defaultScope } ]
]

{ #category : 'opening/closing' }
ClyBrowserMorph >> buildWindow [
	| window |
	window := (SystemWindow labelled: self newWindowTitle) model: self.
	window
		addMorph: self frame: (0@0 extent: 1@1);
		updatePaneColors.
	^window
]

{ #category : 'navigation' }
ClyBrowserMorph >> changeStateBy: aBlock [

	| state navigationFailed |
	navigationStarted ifTrue: [^aBlock value].
	navigationStarted := true.
	navigationFailed := false.
	state := self snapshotState.
	^[
		aBlock on: Error do: [:err |
			navigationFailed := true. "this flag prevents any UI update in case of error"
			err pass]
	] ensure: [
		navigationStarted := false.
		navigationFailed | (state isCurrentStateOf: self) ifFalse: [
			self recordNavigationState: state.
			self updateWindowTitle.
			self rebuildAllTools]]
]

{ #category : 'navigation' }
ClyBrowserMorph >> changeStateOf: aQueryView by: aBlock [

	| newTools |
	self changeStateBy: [
		aBlock value.
		aQueryView changesWasInitiatedByUser ifTrue: [
			newTools := OrderedCollection new.
			tabManager buildToolsOn: newTools for: aQueryView createSelectionContext.
			tabManager desiredSelection: (newTools collect: [:each | each class])	].
	]
]

{ #category : 'opening/closing' }
ClyBrowserMorph >> close [
	| currentWindow |
	currentWindow := self window.

	(currentWindow ownerThatIsA: GroupWindowMorph) ifNotNil: [:group |
		^self okToChange ifTrue: [
			[group removeWindow: currentWindow]
				on: ProvideAnswerNotification do: [:notification |
					"We already got answer from okToChange check above"
					notification resume: true]]].

	currentWindow close
]

{ #category : 'tools support' }
ClyBrowserMorph >> confirmDiscardChanges [

	^self confirm: 'Changes have not been saved.
Is it OK to discard changes?'
]

{ #category : 'context' }
ClyBrowserMorph >> createCommandContext [
	"Some subclasses needs general global context where commands can be attached
	independently from the actual internal widget under focus.
	They should override this method returning appropriate global context instance.
	But by default it is undefined unknown context"

	^ClyUnknownBrowserContext for: self
]

{ #category : 'opening/closing' }
ClyBrowserMorph >> createWindowGroupFrom: currentWindow [
  | group pos ext |
	(currentWindow ownerThatIsA: GroupWindowMorph) ifNotNil: [:existing | ^existing].

	pos := currentWindow position.
	ext := currentWindow extent.
	group := ClyGroupWindowMorph new.
	group addWindow: currentWindow.
	(group openInWindowLabeled: currentWindow label)
 		extent: ext;
		position: pos;
		model: group.
	^group
]

{ #category : 'tools support' }
ClyBrowserMorph >> decorateTool: aBrowserTool [

	plugins do: [ :each | each decorateTool: aBrowserTool  ]
]

{ #category : 'navigation' }
ClyBrowserMorph >> defaultNavigationScope [

	^self systemScope
]

{ #category : 'accessing' }
ClyBrowserMorph >> disablePluginsWhichAreNotIn: aBrowser [

	plugins removeAllSuchThat: [ :each |
		each isAutoActivated and: [ (aBrowser hasPlugin: each) not ] ]
]

{ #category : 'accessing' }
ClyBrowserMorph >> disableSlowPlugins [

	plugins removeAllSuchThat: [ :each | each isSlow]
]

{ #category : 'initialization' }
ClyBrowserMorph >> ensureInitialState [

	(navigationViews anySatisfy: [ :each | each hasRealQuery ])
		ifTrue: [ ^self].

	self prepareDefaultState
]

{ #category : 'tools support' }
ClyBrowserMorph >> focusActiveTab [
	tabManager focusActiveTab
]

{ #category : 'testing' }
ClyBrowserMorph >> hasPlugin: aBrowserPlugin [
	^self hasPluginOf: aBrowserPlugin class
]

{ #category : 'testing' }
ClyBrowserMorph >> hasPluginOf: aBrowserPluginClass [
	^plugins anySatisfy: [ :each | each class = aBrowserPluginClass ]
]

{ #category : 'navigation' }
ClyBrowserMorph >> ignoreNavigationDuring: aBlock [

	navigationStarted ifTrue: [^aBlock value].
	navigationStarted := true.
	aBlock ensure: [ navigationStarted := false ]
]

{ #category : 'initialization' }
ClyBrowserMorph >> initialExtent [
	^ 915@620 * self currentWorld displayScaleFactor
]

{ #category : 'initialization' }
ClyBrowserMorph >> initialize [
	super initialize.
	navigationStarted := false.
	navigationHistory := ClyNavigationHistory new.
	plugins := SortedCollection sortBlock: [ :a :b | a priority <= b priority ].
	self extent: self initialExtent.
	self changeProportionalLayout.
	self initializeToolsPanel.
	self initializeNavigationPanel.
	self layoutChildren
]

{ #category : 'initialization' }
ClyBrowserMorph >> initializeNavigationPanel [

	| eachViewExtent lastViewLeft |
	navigationPanel := PanelMorph new.
	navigationPanel name: 'navigation panel'.
	navigationPanel changeProportionalLayout.
	navigationViews := OrderedCollection new.
	self initializeNavigationViews.

	eachViewExtent := 1.0 / navigationViews size.
	lastViewLeft := 0.0.
	navigationViews do: [ :each | | frame |
		frame := ( lastViewLeft @ 0.0 corner: lastViewLeft + eachViewExtent @ 1.0) asLayoutFrame.
		each == navigationViews last ifFalse: [
			frame := frame rightOffset: -4 ].
		navigationPanel
			addMorph: each
			fullFrame: frame.
		lastViewLeft := lastViewLeft + eachViewExtent ].

	navigationPanel addPaneSplitters
]

{ #category : 'initialization' }
ClyBrowserMorph >> initializeNavigationViews [
	self subclassResponsibility
]

{ #category : 'initialization' }
ClyBrowserMorph >> initializeToolsPanel [

	toolbar := ClyToolbarMorph of: self.
	tabManager := ClyNotebookManager of: self
]

{ #category : 'testing' }
ClyBrowserMorph >> isNavigationPanelFocused [

	^navigationViews anySatisfy: [ :each | each hasKeyboardFocus ]
]

{ #category : 'updating' }
ClyBrowserMorph >> itemsChanged [

	navigationStarted ifTrue: [ ^self ].
	navigationStarted := true.

	[self rebuildToolsForChangedEnvironment] ensure: [ navigationStarted := false ]
]

{ #category : 'keymapping' }
ClyBrowserMorph >> kmDispatcher [

	^ CmdKMDispatcher attachedTo: self
]

{ #category : 'initialization' }
ClyBrowserMorph >> layoutChildren [

	"Set up the typical calypso browser layout with:
	  - package | classes | protocols | methods panels
	  - toolbar
	  - tool (the code editor and so on)"
	self
		addMorph: navigationPanel
		fullFrame: ((0.0 @ 0 corner: 1.0 @ 0.5) asLayoutFrame bottomOffset:
				 toolbar height negated).

	toolPanel := PanelMorph new.
	toolPanel name: 'tools panel'.
	toolPanel
		changeTableLayout;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		listDirection: #topToBottom.
	self
		addMorph: toolPanel
		fullFrame: ((0.0 @ 0.5 corner: 1.0 @ 1.0) asLayoutFrame topOffset:
				 toolbar height negated).

	toolPanel addMorphBack: toolbar.
	toolPanel addMorphBack: tabManager tabMorph.
	
	self addPaneSplitters
]

{ #category : 'navigation' }
ClyBrowserMorph >> navigateBack [
	navigationHistory undoNavigationOf: self
]

{ #category : 'navigation' }
ClyBrowserMorph >> navigateForward [
	navigationHistory redoNavigationOf: self
]

{ #category : 'accessing' }
ClyBrowserMorph >> navigationContexts [

	^ navigationViews collect: [ :each | each createSelectionContext ]
]

{ #category : 'accessing' }
ClyBrowserMorph >> navigationContextsDo: aBlock [

	navigationViews do: [ :each | aBlock value: each createSelectionContext ]
]

{ #category : 'accessing' }
ClyBrowserMorph >> navigationEnvironment [
	^ navigationEnvironment
]

{ #category : 'accessing' }
ClyBrowserMorph >> navigationEnvironment: aNavigationEnvironment [
	navigationEnvironment := aNavigationEnvironment
]

{ #category : 'accessing' }
ClyBrowserMorph >> navigationHistory [
	^ navigationHistory
]

{ #category : 'accessing' }
ClyBrowserMorph >> navigationHistory: aNavigationHistory [
	navigationHistory := aNavigationHistory
]

{ #category : 'accessing' }
ClyBrowserMorph >> navigationViews [
	^ navigationViews
]

{ #category : 'initialization' }
ClyBrowserMorph >> newNavigationView [
	| view |
	view := ClyQueryViewMorph for: self.
	navigationViews add: view.
	^view
]

{ #category : 'updating' }
ClyBrowserMorph >> newWindowTitle [
	self subclassResponsibility
]

{ #category : 'opening/closing' }
ClyBrowserMorph >> okToChange [

	^tabManager okToChange
]

{ #category : 'opening/closing' }
ClyBrowserMorph >> open [
	| window |

	self ensureInitialState.
	window := self openInWindow.
	window model: self.
	self updateWindowTitle.
	window minimumExtent: (650@500.0) scaledByDisplayScaleFactor
]

{ #category : 'opening/closing' }
ClyBrowserMorph >> openAnotherBrowser: aBrowser [
	aBrowser open
]

{ #category : 'opening/closing' }
ClyBrowserMorph >> openInWindow: aWindow [
	| groupWindow myWindow |
	groupWindow := self createWindowGroupFrom: aWindow.

	myWindow := self buildWindow.
	groupWindow addWindow: myWindow.
	myWindow activate.
	myWindow announceOpened
]

{ #category : 'accessing' }
ClyBrowserMorph >> plugins [
	^plugins
]

{ #category : 'initialization' }
ClyBrowserMorph >> prepareDefaultState [
	"The initial state is required state for browser.
	But default state is the state which should be applyed
	when user to not setup any custom state to the browser.
	For example browser can be opened with particulal selected item.
	In that case it will be initial state of browser and default state will be ignored"
	self prepareInitialState
]

{ #category : 'initialization' }
ClyBrowserMorph >> prepareInitialState [
	self subclassResponsibility
]

{ #category : 'initialization' }
ClyBrowserMorph >> prepareInitialStateBy: aBlock [

	navigationHistory ignoreNavigationDuring: [
		self prepareInitialState.
		aBlock valueWithPossibleArgument: self]
]

{ #category : 'updating' }
ClyBrowserMorph >> rebuildAllTools [

	tabManager updateTools.
	self rebuildToolbar
]

{ #category : 'updating' }
ClyBrowserMorph >> rebuildToolbar [

	toolbar updateItems
]

{ #category : 'updating' }
ClyBrowserMorph >> rebuildToolsForChangedEnvironment [

	tabManager updateToolsForChangedEnvironment.
	self rebuildToolbar
]

{ #category : 'navigation' }
ClyBrowserMorph >> recordNavigationState: aBrowserNavigationState [
	navigationHistory recordState: aBrowserNavigationState
]

{ #category : 'accessing' }
ClyBrowserMorph >> removePlugin: aBrowserPlugin [

	plugins removeAllSuchThat: [ :each | each class = aBrowserPlugin class ]
]

{ #category : 'tools support' }
ClyBrowserMorph >> searchDialog [
	^ClyBrowserSearchDialogWindow for: self
]

{ #category : 'initialization' }
ClyBrowserMorph >> setUpAvailablePlugins [

	ClyBrowserPlugin allSubclasses
		select: [ :each | each isAutoActivated ]
		thenDo: [ :each | self addPlugin: each new ]
]

{ #category : 'updating' }
ClyBrowserMorph >> setWindowTitle: aString in: aWindow [

	aWindow setLabel: aString
]

{ #category : 'navigation' }
ClyBrowserMorph >> snapshotState [

	^ClyBrowserState of: self
]

{ #category : 'navigation' }
ClyBrowserMorph >> spawnBrowser: aBrowserClass withState: navigationBlock [
	| browser |
	browser := aBrowserClass on: navigationEnvironment systemScope: self systemScope.
	browser disablePluginsWhichAreNotIn: self.

	browser prepareInitialStateBy: navigationBlock.
	self openAnotherBrowser: browser.
	browser wasSpawnedFrom: self.
	^browser
]

{ #category : 'navigation' }
ClyBrowserMorph >> switchFocusToNextPane [

	| focused next |
	focused := navigationViews detect: [ :each | each hasKeyboardFocus ] ifNone: [ ^self ].
	next := navigationViews after: focused ifAbsent: [ ^self focusActiveTab ].
	next takeKeyboardFocus
]

{ #category : 'navigation' }
ClyBrowserMorph >> switchFocusToPreviousPane [

	| focused next |
	focused := navigationViews detect: [ :each | each hasKeyboardFocus ] ifNone: [ ^self ].
	next := navigationViews before: focused ifAbsent: [ ^self ].
	next takeKeyboardFocus
]

{ #category : 'accessing' }
ClyBrowserMorph >> system [
	^ navigationEnvironment system
]

{ #category : 'accessing' }
ClyBrowserMorph >> systemScope [
	^systemScope ifNil: [systemScope := navigationEnvironment systemScope]
]

{ #category : 'accessing' }
ClyBrowserMorph >> systemScope: aSystemScope [
	systemScope := aSystemScope
]

{ #category : 'accessing' }
ClyBrowserMorph >> tabManager [
	^tabManager
]

{ #category : 'tools support' }
ClyBrowserMorph >> toggleFullWindowTabs [

	| tabMorph |
	tabMorph := tabManager tabMorph.
	(submorphs includes: tabMorph )
		ifFalse: [ self addMorph: tabMorph fullFrame: LayoutFrame identity]
		ifTrue: [ toolPanel addMorphBack: tabMorph ]
]

{ #category : 'accessing' }
ClyBrowserMorph >> toolbar [
	^toolbar
]

{ #category : 'updating' }
ClyBrowserMorph >> update [

	navigationViews do: [ :each | each update ]
]

{ #category : 'updating' }
ClyBrowserMorph >> updateWindowTitle [

	self window ifNotNil: [ :w |
		self setWindowTitle: self newWindowTitle in: w.
		"in the case when window is managed as tab by window group
		we should set up top group window label too"
		(self ownerThatIsA: GroupWindowMorph) ifNotNil: [:group |
			group window ifNotNil: [:mainWindow |
				self setWindowTitle: w labelString in: mainWindow]]
	]
]

{ #category : 'navigation' }
ClyBrowserMorph >> wasSpawnedFrom: aBrowser [

	self recordNavigationState:  (ClyAccrossWindowNavigationState from: aBrowser)
]

{ #category : 'opening/closing' }
ClyBrowserMorph >> windowIsClosing [

	navigationViews do: [ :each | each windowIsClosing ].
	tabManager windowIsClosing.
	self window ifNotNil: [ :w |
		w
			removeMorph: self;
			updatePanesFromSubmorphs;
			model: nil]
]

{ #category : 'tools support' }
ClyBrowserMorph >> withTool: aToolClass do: aBlock [
	^tabManager withTool: aToolClass do: aBlock
]
