"
I am a root of browser tabs hierarchy.
My subclasses should implement single method #build where they should create and add required widgets.

The build is always executed in background due to the TabManagerMorph logic.

Tab manager collects all subclasses which should be opened for current browser state.
It lookups tools annotated with ClyTabActivationStrategy for current browser context.

So to enable any tool in the browser you should annotated it with еру strategy for appropriate context where tool should be opened.
For example:

	ClyMethodCodeEditorTool class>>methodTabActivation
		<classAnnotation>
		^ClyTabActivationStrategy for: ClyMethod asCalypsoItemContext

It will open method editor when method is selected in the browser.	

Subclasses can define extra condition to check that they can be opened in particular browser context instance.
They should override class side method: 

- shouldBeActivatedInContext: aBrowserContext

It returns true by default.

Also abstract subclasses are never opened in the browser. By default the tool is abstract if it has subclasses.
The class side method #isAbstact should be overridden if this default condition is not valid.

Tab instances are created with browser context 

	ClyBrowserTool for: aBrowser inContext: aBrowserContext
	
Subclasses should implement initialization methods to retrieve required state from the given context:

- setUpModelFromContext
- setUpParametersFromModel

Last method is also used to update tool when model is changed.

Subclasses should override method #isSimilarTo: to compare with another tool which has same parameters.
For example ClyMethodCodeEditorTool checks that other method editor is based on same method.
#isSimilarTo: method is used by ClyTabManager to update existing tabs in new browser state. If similar tool is already opened in browser then it will be not replaced by new instance.
Look ClyTabManager comments for details.

Tab manager never close dirty tabs when browser selection is changed. Instead if ask the tool to indicate different browser context.
To support this logic tools should implement following methods: 

- belongsToCurrentBrowserContext
For example method editor checks that browser still selects editing method.

- warnUserAboutMyContext
Here the tool is supposed to reset any indication of different context. It happens when user move selection back where dirty tab should be opened again. 

- warnUserAboutDifferentContext
Here the tool can indicate that it is now is untouched from the current browser state. For example when user selects new method while dirty method is still opened. 

- warnUserAboutChangedContext
It just merges previous two methods by testing for #belongsToCurrentBrowserContext.

To support dirty state subclasses should implement method #hasUnacceptedEdits. It is false by default.
And indication of dirtiness can be overridden in the method #updateDirtyState. By default it just adds * to the tab title. 

- hasUnacceptedEdits (false by default)
- updateDirtyState

So tabs are based on some models. For example method editor model is a method. 
When tool model is removed from system (method is removed) the tab should be automatically closed.
Tab manager detects such conditions using following method which tools should implement:

- belongsToRemovedBrowserContext

For example method editor checks that editing method was removed from system.

So the model of the tool can be modified and therefore the tool should implement update logic using following methods:

- attachToSystem
It should subscribe the tool for model changes.

- detachFromSystem
It should unsubscribe the tool from the model.

In addition I provide method #update which subclasses should reuse in the update logic of the method changes.
#update refreshes basic visual properties of tool retrieved from the model and it rebuilds status bar.

TabManager orders retrieved tools using method #tabOrder.
Also manager chooses what tab should be selected first. It selects the tool with highest #activationPriority which is equal to #tabOrder by default.
In general tab selection logic is more complex. Look at ClyTabManager comments for details.

To specify tab title and icon subclasses can implement following methods: 

- defaultIconName
- defaultIcon (to be able create dynamic icon instance when simple icon name is not enough)
- defaultTitle (by default it is the name of current selected item in the browser or just a tool class name if nothing selected)

I provide status bar. And subclasses can add items to it in the method: 

- fillStatusBar

Any tool can be decorated by browser plugins which can inject other widgets or modify general style of the tool.
Plugins add decorators using following method: 
	
	aBrowserTool addDecorator: aBrowserToolDecorator

And at the end of building process I apply all configured decorators:

	self applyDecorations
	
Notice that full tab building logic is implemented in the method: 

- buildAndDecorate

To remove the tool from browser just call #removeFromBrowser method.

Internal Representation and Key Implementation Points.

    Instance Variables
	browser:		<ClyBrowser>
	containerTab:		<TabMorph>
	context:		<ClyBrowserContext>
	decorators:		<OrderedCollection of<ClyBrowserToolDecorator>>
	isDirty:		<Boolean>
	isManagedByUser:		<Boolean>
	statusBar:		<ClyStatusBar>
"
Class {
	#name : #ClyBrowserToolMorph,
	#superclass : #BorderedMorph,
	#instVars : [
		'browser',
		'context',
		'decorators',
		'containerTab',
		'isDirty',
		'isManagedByUser',
		'statusBar'
	],
	#category : #'Calypso-Browser-UI'
}

{ #category : #'instance creation' }
ClyBrowserToolMorph class >> for: aBrowser inContext: aBrowserContext [

	^self new 
		browser: aBrowser;
		context: aBrowserContext;
		setUpModelFromContext;
		yourself
]

{ #category : #testing }
ClyBrowserToolMorph class >> isAbstract [
	^self subclasses notEmpty
]

{ #category : #testing }
ClyBrowserToolMorph class >> shouldBeActivatedInContext: aBrowserContext [
	"Subclasses can define extra condition 
	to check that they can be opened in particular browser context instance.
	By default it is always true"
	^true
]

{ #category : #accessing }
ClyBrowserToolMorph class >> tabOrder [
	^1000
]

{ #category : #accessing }
ClyBrowserToolMorph >> activationPriority [
	^self tabOrder
]

{ #category : #accessing }
ClyBrowserToolMorph >> activationPriorityInNonActiveContext [
	"when tool is shown in not active browser context it should be in less priority for activating tabs"
	^isManagedByUser 
		ifTrue: [ self activationPriority ] 
		ifFalse: [Float negativeInfinity ]
]

{ #category : #accessing }
ClyBrowserToolMorph >> activeColor [
	^self theme backgroundColor
]

{ #category : #accessing }
ClyBrowserToolMorph >> addDecorator: aBrowserToolDecorator [
	aBrowserToolDecorator browserTool: self.
	decorators add: aBrowserToolDecorator
]

{ #category : #testing }
ClyBrowserToolMorph >> allowsDifferentActivationPriority [
	^true
]

{ #category : #building }
ClyBrowserToolMorph >> applyDecorations [
	decorators do: [ :each | each decorateTool ]
]

{ #category : #controlling }
ClyBrowserToolMorph >> attachToSystem [
]

{ #category : #testing }
ClyBrowserToolMorph >> belongsToCurrentBrowserContext [
	"my subclasses must define condition which means that tool has context which is the same as current browser context.
	This method is not subclass responsibility to not break browser when new tool class is created"
	^true
]

{ #category : #testing }
ClyBrowserToolMorph >> belongsToRemovedBrowserContext [
	^false
]

{ #category : #accessing }
ClyBrowserToolMorph >> browser [
	^ browser
]

{ #category : #accessing }
ClyBrowserToolMorph >> browser: aBrowser [
	browser := aBrowser
]

{ #category : #updating }
ClyBrowserToolMorph >> browserContextWasChanged [

	self warnUserAboutChangedContext.
	self rebuildStatusBar
]

{ #category : #building }
ClyBrowserToolMorph >> build [
	self subclassResponsibility 
]

{ #category : #building }
ClyBrowserToolMorph >> buildAndDecorate [
	self isBuilt ifTrue: [ ^self ] "somehow tab manager can call building process multiple times".
	self setUpParametersFromModel.
	self decorateContainerTab.
	self build. 
	self buildStatusBar.
	self applyDecorations.
	self attachToSystem
]

{ #category : #building }
ClyBrowserToolMorph >> buildStatusBar [

	statusBar := ClyStatusBarMorph new.
	statusBar 
		width: browser width. "it is required because tool is build in background"
"		color: self theme windowColor."
	self addMorphBack: statusBar.
	
	self fillStatusBar
]

{ #category : #controlling }
ClyBrowserToolMorph >> cleanAfterRemove [
	self detachFromSystem.
	decorators do: [ :each | each toolWasRemoved ]
]

{ #category : #accessing }
ClyBrowserToolMorph >> containerTab [
	^ containerTab
]

{ #category : #accessing }
ClyBrowserToolMorph >> containerTab: anObject [
	containerTab := anObject
]

{ #category : #accessing }
ClyBrowserToolMorph >> context [
	^ context
]

{ #category : #accessing }
ClyBrowserToolMorph >> context: aBrowserContext [
	context := aBrowserContext
]

{ #category : #copying }
ClyBrowserToolMorph >> copyForBrowserStateSnapshot [

	^self shallowCopy
		resetStateForSnapshot;
		yourself
]

{ #category : #building }
ClyBrowserToolMorph >> createTab [
	| menu |
	menu := CmdMenu activatedBy: ClyBrowserTabCommandActivation.
	menu buildInContext: context.
	
	containerTab := TabMorph
		label: self defaultTitle
		icon: self defaultIcon
		retrievingBlock: [self buildAndDecorate]
		actions: menu buildBrowserTabActions.
	
	containerTab model: self.
	^containerTab
]

{ #category : #building }
ClyBrowserToolMorph >> decorateContainerTab [
]

{ #category : #initialization }
ClyBrowserToolMorph >> defaultBorderWidth [ 
	^0
]

{ #category : #initialization }
ClyBrowserToolMorph >> defaultColor [
	^Color transparent
]

{ #category : #initialization }
ClyBrowserToolMorph >> defaultIcon [
	self defaultIconName ifNil: [ ^nil ].
	
	^self iconNamed: self defaultIconName
]

{ #category : #initialization }
ClyBrowserToolMorph >> defaultIconName [
	^nil
]

{ #category : #initialization }
ClyBrowserToolMorph >> defaultTitle [
	^context hasSelectedItems 
		ifTrue: [ context lastSelectedItem name]
		ifFalse: [ self class name]
]

{ #category : #controlling }
ClyBrowserToolMorph >> deselectTab [
	
	containerTab removeFromSelection
]

{ #category : #controlling }
ClyBrowserToolMorph >> detachFromSystem [
]

{ #category : #building }
ClyBrowserToolMorph >> fillStatusBar [

	self belongsToCurrentBrowserContext ifFalse: [ 
		statusBar addContextItem: (ClyToolContextNavigatorMorph for: self) ]
]

{ #category : #testing }
ClyBrowserToolMorph >> hasUnacceptedEdits [
	^false
]

{ #category : #initialization }
ClyBrowserToolMorph >> initialize [
	super initialize.
	decorators := OrderedCollection new.
	isManagedByUser := false.
	isDirty := false.
	self
		changeTableLayout;
		listDirection: #topToBottom;
		hResizing: #spaceFill;
		vResizing: #spaceFill
	
]

{ #category : #testing }
ClyBrowserToolMorph >> isBuilt [
	containerTab ifNil: [ ^false ].
	^containerTab morph == self
]

{ #category : #testing }
ClyBrowserToolMorph >> isExtraSelectionRequested [
	"It checks that users wants multiple selected tabs.
	So cmd click on table just adds new selected items and keep existed selected.
	And this method allow tab manager to keep current tool selected in that case.
	In addition tab manager will force all similar tools (of same class) to be selected.
	So cmd+click on method pane leads to multiple methods selected in the table and in the tabs"

	"Following mouse logic ensures that we are under condition when mouseDown+cmd was produced.
	(it should be simple #meta modifier. But we don't have it"
	| lastEvent |
	lastEvent := ActiveHand lastEvent.
	^ lastEvent isMouse & lastEvent isMouseDown 
		and: [Sensor commandKeyPressed or: [ Sensor controlKeyPressed ]]
]

{ #category : #accessing }
ClyBrowserToolMorph >> isManagedByUser [
	^ isManagedByUser
]

{ #category : #accessing }
ClyBrowserToolMorph >> isManagedByUser: anObject [
	isManagedByUser := anObject
]

{ #category : #testing }
ClyBrowserToolMorph >> isSimilarTo: anotherBrowserTool [
	^self class = anotherBrowserTool class
]

{ #category : #testing }
ClyBrowserToolMorph >> isTabSelected [
	^containerTab selected
]

{ #category : #controlling }
ClyBrowserToolMorph >> okToClose [
	self hasUnacceptedEdits ifFalse: [ ^true ].
	
	^browser confirmDiscardChanges
]

{ #category : #printing }
ClyBrowserToolMorph >> printContext [
	^''
]

{ #category : #building }
ClyBrowserToolMorph >> rebuildStatusBar [
	self isBuilt ifFalse: [ ^self ].
	
	statusBar clear.
	self fillStatusBar
]

{ #category : #controlling }
ClyBrowserToolMorph >> removeFromBrowser [

	containerTab delete
]

{ #category : #initialization }
ClyBrowserToolMorph >> resetStateForSnapshot [
	owner := nil.
	submorphs := EmptyArray.
	extension := nil.
	browser := nil.
	context := nil.
	containerTab := nil.
	statusBar := nil.
	isDirty := false.
	decorators := nil
]

{ #category : #controlling }
ClyBrowserToolMorph >> restoreBrowserState [
	context restoreBrowserState
]

{ #category : #controlling }
ClyBrowserToolMorph >> selectAsExtraTab [
	containerTab addToSelection
]

{ #category : #controlling }
ClyBrowserToolMorph >> selectAsMainTab [
	containerTab selected: true.
	
	"this is hack to fix blinking during tabs activation"
	(browser tabManager tabMorph instVarNamed: #contentsWrapper) color: self activeColor
]

{ #category : #initialization }
ClyBrowserToolMorph >> setUpDefaultIcon [
	containerTab icon: self defaultIcon
]

{ #category : #initialization }
ClyBrowserToolMorph >> setUpModelFromContext [

]

{ #category : #initialization }
ClyBrowserToolMorph >> setUpParametersFromModel [

]

{ #category : #accessing }
ClyBrowserToolMorph >> statusBar [
	^statusBar
]

{ #category : #accessing }
ClyBrowserToolMorph >> tabOrder [
	^ self class tabOrder
]

{ #category : #updating }
ClyBrowserToolMorph >> triggerUpdate [
	self update
]

{ #category : #updating }
ClyBrowserToolMorph >> update [
	self setUpParametersFromModel.
	self decorateContainerTab.
	self rebuildStatusBar.
	self warnUserAboutChangedContext
]

{ #category : #controlling }
ClyBrowserToolMorph >> updateDirtyState [

	| currentLabel |
	self hasUnacceptedEdits == isDirty ifTrue: [ ^self].

	"we want escape getting label from actual model because it simplified things 
	and in remote scenario it will not make any request to server"
	currentLabel := containerTab label morph contents.
	
	isDirty := self hasUnacceptedEdits.
	isDirty 
		ifTrue: [ containerTab label: '*', currentLabel]
		ifFalse: [ 
			(currentLabel beginsWith: '*') ifTrue: [ 
					containerTab label: currentLabel allButFirst ]]
]

{ #category : #controlling }
ClyBrowserToolMorph >> waitBuildCompletion [
	containerTab waitReadyMorphFor: 50 milliSeconds
]

{ #category : #testing }
ClyBrowserToolMorph >> wantsStayInDifferentContext [
	self isBuilt ifFalse: [ ^false ].
	self hasUnacceptedEdits ifTrue: [ ^true ].
	self isTabSelected & self isExtraSelectionRequested ifTrue: [ ^true ].
	self belongsToRemovedBrowserContext ifTrue: [ ^false ].
	^isManagedByUser
]

{ #category : #controlling }
ClyBrowserToolMorph >> warnUserAboutChangedContext [

	self belongsToCurrentBrowserContext
		ifTrue: [ self warnUserAboutMyContext ]
		ifFalse: [ self warnUserAboutDifferentContext ]
]

{ #category : #controlling }
ClyBrowserToolMorph >> warnUserAboutDifferentContext [
	containerTab icon: (self iconNamed: #uncommentedClass)
]

{ #category : #controlling }
ClyBrowserToolMorph >> warnUserAboutMyContext [

	self setUpDefaultIcon
]

{ #category : #controlling }
ClyBrowserToolMorph >> whenReadyDo: aBlock [

	containerTab waitReadyMorphFor: 10 seconds. "There is no other way to ensure async tab is ready"
	aBlock value: self
]
