Class {
	#name : #ClyNotebookManager,
	#superclass : #Object,
	#instVars : [
		'browser',
		'tools',
		'updatingStarted',
		'selectionPriorities',
		'desiredSelection',
		'shouldSkipCurrentDesiredSelection',
		'tabMorph'
	],
	#category : #'Calypso-Browser-Tabs'
}

{ #category : #'instance creation' }
ClyNotebookManager class >> of: aBrowser [
	^self new 
		browser: aBrowser
]

{ #category : #private }
ClyNotebookManager >> activationPriorityOf: aBrowserTool [
	
	| priority |
	aBrowserTool belongsToCurrentBrowserContext ifFalse: [ 
		^aBrowserTool activationPriorityInNonActiveContext].
	
	priority := self overridenPriorityOf: aBrowserTool.
	^(desiredSelection includes: aBrowserTool class)
		ifTrue: [ priority * 1000 ]
		ifFalse: [ priority ]
]

{ #category : #accessing }
ClyNotebookManager >> activeStatusBar [

	^self selectedTools first statusBar
]

{ #category : #private }
ClyNotebookManager >> addTool: aBrowserTool [
	| tab |
	
	tools add: aBrowserTool.
	"tab build is performed in background when owner is not exist yet. But we need proper width to perform kind of styling/formatting for tool if needed"
	aBrowserTool width: tabMorph width.
	tab := aBrowserTool addNotebookPageOn: self tabMorph.
	self flag: #TODO. "This needs to be added to the tab morph"
	"self tabMorph pages sort: [ :a :b | a model tabOrder <= b model tabOrder ]."
	self flag: #TODO.
	"tab when: #tabDeleted send: #tabDeleted: to: self.
	tab when: #tabSelected send: #tabSelected: to: self.
	tab on: #doubleClick send: #toggleFullWindowTabs to: browser.
	""Following code is required to fix the problem 
	when doubleClick handler of tab label disables event processing by its parent, TabMorph""
	tab label on: #mouseDown send: #mouseDown: to: tab.
	tab label on: #mouseUp send: #mouseUp: to: tab."
]

{ #category : #accessing }
ClyNotebookManager >> browser [
	^ browser
]

{ #category : #accessing }
ClyNotebookManager >> browser: anObject [
	browser := anObject
]

{ #category : #private }
ClyNotebookManager >> buildToolsOn: toolsList for: aBrowserContext [
	| tool |
	ClyTabActivationStrategy 
		activeInstancesInContext: aBrowserContext
		do: [ :strategy | 
			tool := strategy createToolFor: browser inContext: aBrowserContext.
			browser decorateTool: tool.
			toolsList add: tool ]
]

{ #category : #accessing }
ClyNotebookManager >> countToolsSimilarTo: aBrowserTool [

	^ tools count: [ :each | each class = aBrowserTool class ]
]

{ #category : #accessing }
ClyNotebookManager >> desiredSelection [
	^ desiredSelection
]

{ #category : #accessing }
ClyNotebookManager >> desiredSelection: toolClasses [
	| currentSelection |
	desiredSelection := toolClasses asIdentitySet.
	shouldSkipCurrentDesiredSelection ifFalse: [ 
		currentSelection := self selectedTools collect: [ :each | each class ].
		desiredSelection addAll: currentSelection].
	shouldSkipCurrentDesiredSelection := false "it is one time option" 
]

{ #category : #'focus management' }
ClyNotebookManager >> focusActiveTab [
	| activeTab |

	activeTab := self selectedTools detectMax: [ :each | 
		self activationPriorityOf: each ].
		
	activeTab ifNotNil: [ self tabMorph page: activeTab ]
]

{ #category : #initialization }
ClyNotebookManager >> initialize [
	super initialize.
	
	tools := SortedCollection sortBlock: [ :a :b | a tabOrder <= b tabOrder ].
	selectionPriorities := IdentityDictionary new.
	desiredSelection := IdentitySet new.
	shouldSkipCurrentDesiredSelection := false.
	updatingStarted := false.
	tabMorph := SpNotebookMorph new.
	"tabMorph pageMorphs: (SortedCollection sortBlock: [ :a :b | a model tabOrder <= b model tabOrder ])."
	tabMorph announcer
		when: SpNotebookPageChanged 
		send: #pageChanged: 
		to: self.
	tabMorph 
		hResizing: #spaceFill;
		vResizing: #spaceFill
]

{ #category : #'event handling' }
ClyNotebookManager >> okToChange [

	(tools anySatisfy: #hasUnacceptedEdits) ifFalse: [ ^ true ].
	^browser confirmDiscardChanges 
]

{ #category : #private }
ClyNotebookManager >> overridenPriorityOf: aBrowserTool [
	
	^selectionPriorities at: aBrowserTool class ifAbsent: [ aBrowserTool activationPriority  ]
]

{ #category : #private }
ClyNotebookManager >> pageChanged: ann [
	| selectedTool oldSelectedTools browserState |
	
	updatingStarted ifTrue: [ ^ self ].
	selectedTool := ann page model.
	oldSelectedTools := ann oldPage model 
		ifNotNil: [ :aTool | { aTool } ] 
		ifNil: [ #() ].
	"happens somehow during tabs rebuild".
	(tools includes: selectedTool) ifFalse: [ ^ self ]. 
	
	oldSelectedTools ifEmpty: [ ^ self ].
	desiredSelection removeAll.
	browserState := browser snapshotState.
	browserState selectedTabs: oldSelectedTools.
	browser recordNavigationState: browserState.
	
	self 
		swapPrioritiesBetween: selectedTool
		and: oldSelectedTools first
]

{ #category : #private }
ClyNotebookManager >> removeTool: aTool [

	self flag: #TODO. "Why sometimes is not there?"
	(self tabMorph pages includes: aTool containerTab)
		ifTrue: [ self tabMorph removePage: aTool containerTab ].
	self tabDeleted: aTool containerTab
]

{ #category : #testing }
ClyNotebookManager >> requiresNewDesiredSelection [
	desiredSelection ifEmpty: [ ^false ].
	
	^(self selectedTools allSatisfy: [ :each | desiredSelection includes: each class]) not
]

{ #category : #updating }
ClyNotebookManager >> restoreBrowserState: aBrowserState [
	| existingTools |
	"Browser state do only hold selected tabs. 
	So first we should restore all tools in new context and then restore selection"
	self updateTools.
	
	existingTools := aBrowserState selectedTabs
		collect: [ :oldTool | tools detect: [ :each | each isSimilarTo: oldTool ] ifNone: [ nil ] ]
		thenSelect: #notNil.
		
	existingTools ifEmpty: [ ^ self ].
	existingTools first selectAsMainTab.
	existingTools allButFirstDo: [ :each | each selectAsExtraTab ]
]

{ #category : #private }
ClyNotebookManager >> restoreSelectedTools: selectedTools [
	| mainTool extraTools |

	tools ifEmpty: [ ^ self ].
	mainTool := self selectMainTool.
	
	extraTools := selectedTools reject: [ :each | each class = mainTool class ].
	extraTools size = selectedTools size 
		ifTrue: [ extraTools := #() ] "if main tool not existed before then we reset previously selected extra tools"
		ifFalse: [ 
			"To allow multiple selected tabs by cmd+click on table"
			mainTool isExtraSelectionRequested 
				ifTrue: [ extraTools add: mainTool ] ].
	(tools copyWithout: mainTool) do: [ :currentTool | 
		"Generally if previously selected extra tab is found in new tools then it should be selected.
		Other tools should be deselected"
		extraTools 
			detect: [ :oldTool | oldTool class = currentTool class  ]
			ifFound: [ self restoreSelectionOfExtraTool: currentTool ]
			ifNone: [ "currentTool deselectTab" ] ].
		
	extraTools ifNotEmpty: [ 
		"last selected tab is looks different from others. We want main tool looks like last selected tab"
		mainTool deselectTab.
		mainTool selectAsExtraTab ].

	"mainTool waitBuildCompletion" "to avoid blinking we try to wait a little bit until mainly selected tab wiull be build. When it is built fast it will look like no background building was happen"
]

{ #category : #private }
ClyNotebookManager >> restoreSelectionOfExtraTool: aBrowserTool [
	
	aBrowserTool hasUnacceptedEdits ifFalse: [ aBrowserTool selectAsExtraTab. ^self ].
	
	(self countToolsSimilarTo: aBrowserTool) = 1
		ifTrue: [ aBrowserTool selectAsExtraTab ]
		ifFalse: [aBrowserTool deselectTab]
]

{ #category : #private }
ClyNotebookManager >> selectMainTool [
	| desiredTools mainTool |
	
	desiredTools := tools select: [ :each | desiredSelection includes: each class ].
	desiredTools ifEmpty: [ desiredTools := tools ].

	mainTool := desiredTools detectMax: [ :each | 
		self activationPriorityOf: each ].
	
	self tabMorph page: mainTool containerTab.
	
	^ mainTool
]

{ #category : #accessing }
ClyNotebookManager >> selectedTools [

	^ self tabMorph page
		ifNotNil: [ :aPage | { aPage } ]
		ifNil: [ #() ]
]

{ #category : #private }
ClyNotebookManager >> selectsTools: toolsArray [
	
	| selectedTools |
	selectedTools := self selectedTools.
	selectedTools size = toolsArray size ifFalse: [ ^false ].
	
	^selectedTools allSatisfy: [ :existingTool | 
		toolsArray anySatisfy: [ :each | existingTool isSimilarTo: each ]]
]

{ #category : #accessing }
ClyNotebookManager >> shouldSkipCurrentDesiredSelection [
	^ shouldSkipCurrentDesiredSelection
]

{ #category : #accessing }
ClyNotebookManager >> shouldSkipCurrentDesiredSelection: anObject [
	shouldSkipCurrentDesiredSelection := anObject
]

{ #category : #accessing }
ClyNotebookManager >> skipCurrentDesiredSelection [
	shouldSkipCurrentDesiredSelection := true
]

{ #category : #private }
ClyNotebookManager >> swapPrioritiesBetween: aTool1 and: aTool2 [
	| priority1 priority2 |
	
	aTool1 allowsDifferentActivationPriority ifFalse: [ ^self ].
	aTool2 allowsDifferentActivationPriority ifFalse: [ ^self ].
	
	priority1 := self overridenPriorityOf: aTool1.
	priority2 := self overridenPriorityOf: aTool2.	
	selectionPriorities at: aTool1 class put: priority2.
	selectionPriorities at: aTool2 class put: priority1.
]

{ #category : #private }
ClyNotebookManager >> tabDeleted: aTab [
	| removedTool |

	removedTool := aTab model.
	(tools includes: removedTool) ifFalse: [ ^ self ].
	tools remove: removedTool.
	removedTool cleanAfterRemove	
]

{ #category : #accessing }
ClyNotebookManager >> tabMorph [

	^ tabMorph
]

{ #category : #accessing }
ClyNotebookManager >> tabMorph: anObject [

	tabMorph := anObject
]

{ #category : #accessing }
ClyNotebookManager >> tools [
	^ tools
]

{ #category : #accessing }
ClyNotebookManager >> tools: anObject [
	tools := anObject
]

{ #category : #updating }
ClyNotebookManager >> updateTabsWith: newTools [
	| toRemove toInstall |
	
	toRemove := OrderedCollection withAll: tools.
	toInstall := OrderedCollection new.
	newTools do: [ :new | 
		tools 
			detect: [ :existing | existing isSimilarTo: new ]
			ifFound: [ :existing | toRemove remove: existing ]
			ifNone: [ toInstall add: new ] ].

	toRemove 
		reject: [ :each | each wantsStayInDifferentContext ]
		thenDo: [ :each | self removeTool: each ].
	tools do: [ :each | each browserContextWasChanged ].
	toInstall do: [ :each | self addTool: each ]
]

{ #category : #updating }
ClyNotebookManager >> updateTools [
	| newTools selectedTools currentTools needsNewSelection |
	
	self updateToolsBy: [
		newTools := OrderedCollection new.	
		browser navigationContextsDo: [ :each | self buildToolsOn: newTools for: each ].
		needsNewSelection := self requiresNewDesiredSelection.
		selectedTools := self selectedTools.
		currentTools := tools copy.
		self updateTabsWith: newTools.
		tools ~= currentTools | needsNewSelection ifTrue: [ self restoreSelectedTools: selectedTools ] ]
]

{ #category : #private }
ClyNotebookManager >> updateToolsBy: aBlock [
	updatingStarted := true.
	aBlock ensure: [ updatingStarted := false ]
]

{ #category : #updating }
ClyNotebookManager >> updateToolsForChangedEnvironment [
	| currentState |
	currentState := OrderedCollection new: tools size.
	tools do: [ :each | 
		currentState add: each -> each isManagedByUser.
		each isManagedByUser: true].
	
	self updateTools.
	
	currentState do: [ :each | each key isManagedByUser: each value ]
]

{ #category : #'event handling' }
ClyNotebookManager >> windowIsClosing [

	tools do: [ :each | each cleanAfterRemove ]
]

{ #category : #accessing }
ClyNotebookManager >> withTool: aToolClass do: aBlock [
	| tool |
	tool := tools detect: [ :each | each isKindOf: aToolClass ].
	^ tool whenReadyDo: aBlock
]
