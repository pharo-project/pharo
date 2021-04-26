"
I manage tabs in the browser.
Any tab in browser is represented by ClyBrowserTool subclasses.
And my responsibility is to show all appropriate tools which are relevant to the current browser context (state). 
`#updateTools` method is doing that. 

The logic is simple: 
When selection (browser context) is changed then browser collects new tools which should be opened in that new browser state. 
Then it removes all old tools and open all new tools. 
But there is special case when new collected tool is already opened. In that case such new tool will be not used. And existing tool will be not removed. So it will stay opened.

I use `#isSimilarTo:` tab method to detect that new collected tool is already opened (the browser already shows similar tool).
By default #isSimilarTo: simply checks the class of given tool. My subclases should redefine it when they include extra state because otherwise new tool instance will never replace old one (browser will think that it is already opened).

There are cases when existing tools are not closed when selection is changed. For example when method editor is dirty and you select another method.
In that case dirty method will indicate that it is now do not belongs to the context of browser.
Tools implement method #belongsToCurrentBrowserContext to support this logic.
For example method editor checks that browser still selects editing method.

There is one complex part of my behaviour: the way how I choose what tab should be selected.
In simple cases I just select the tab with lagest value of #activationPriority. But it is not enough.
Problem that user want to keep current selected tab (the kind of tab) when he selects another item in the table.
For example in full browser user can select class. It will automatically selects the tab with class definition because it has the most activation priority.
But then user can select class comment tab and switch to another class. The desired behaviour is to keep comment tab selected for this newly selected class.

And for this logic I maintain desired set of selected tool in the variable desiredSelection.
It adds and removes items when user manually selects tabs.
But in addition browser fills it with tools which are relevant for manually selected table.
Every time user selects new item in the table the browser collects tools which are relevant for this new selection and it passes them to me as new desired selection. 

So at the end I always select tab with most activation priority which exists in desiredSelection list.

By default activationPriority is equal to `#tabOrder` which defines general order between tabs.

My instances are created on the browser: 

```
	ClyTabManager of: aBrowser
```	
"
Class {
	#name : #ClyNotebookManager,
	#superclass : #Object,
	#instVars : [
		'browser',
		'tabMorph',
		'tools',
		'updatingStarted',
		'selectionPriorities',
		'desiredSelection',
		'shouldSkipCurrentDesiredSelection'
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

	aBrowserTool belongsToCurrentBrowserContext 
		ifFalse: [ ^ aBrowserTool activationPriorityInNonActiveContext ].
	
	priority := self overridenPriorityOf: aBrowserTool.
	^ (desiredSelection includes: aBrowserTool class)
		ifTrue: [ priority * 1000 ]
		ifFalse: [ priority ]
]

{ #category : #accessing }
ClyNotebookManager >> activeStatusBar [

	^ self selectedTools first statusBar
]

{ #category : #private }
ClyNotebookManager >> addTool: aBrowserTool [
	| tab |
	
	tools add: aBrowserTool.
	"tab build is performed in background when owner is not exist yet. But we need proper width to perform kind of styling/formatting for tool if needed"
	aBrowserTool width: tabMorph width.
	tab := aBrowserTool addNotebookPageOn: tabMorph.
	tab 
		onClose: [ self closeTab: tab ];
		onDoubleClick: [ browser toggleFullWindowTabs ]
]

{ #category : #updating }
ClyNotebookManager >> basicUpdateTools [
		| newTools needsNewSelection selectedTools currentTools |
		newTools := OrderedCollection new.	
		browser navigationContextsDo: [ :each | self buildToolsOn: newTools for: each].
		needsNewSelection := self requiresNewDesiredSelection.
		selectedTools := self selectedTools.
		currentTools := tools copy.

		self updateTabsWith: newTools.
		(needsNewSelection or: [ tools ~= currentTools ]) 
			ifTrue: [ self restoreSelectedTools: selectedTools].
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
	ClyTabActivationStrategyAnnotation 
		activeInstancesInContext: aBrowserContext
		do: [ :strategy | 
			tool := strategy createToolFor: browser inContext: aBrowserContext.
			browser decorateTool: tool.
			toolsList add: tool ]
]

{ #category : #private }
ClyNotebookManager >> closeTab: tab [
	
	tab model okToClose ifFalse: [ ^ self ].
	self tabMorph removePage: tab.
	self tabDeleted: tab
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
		desiredSelection addAll: currentSelection ].
	shouldSkipCurrentDesiredSelection := false "it is one time option" 
]

{ #category : #'focus management' }
ClyNotebookManager >> focusActiveTab [
	| activeTool |

	activeTool := self selectedTools detectMax: [ :each | 
		self activationPriorityOf: each ].
		
	activeTool ifNotNil: [ tabMorph page: activeTool containerTab ]
]

{ #category : #initialization }
ClyNotebookManager >> initialize [
	super initialize.
	
	tools := SortedCollection sortBlock: [ :a :b | a tabOrder <= b tabOrder ].
	selectionPriorities := IdentityDictionary new.
	desiredSelection := IdentitySet new.
	shouldSkipCurrentDesiredSelection := false.
	updatingStarted := false.
	tabMorph := ClyNotebookMorph new.
	tabMorph useSortedTabsBy: [ :a :b | (self tabOrderOf: a) <= (self tabOrderOf: b) ].
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
	(tabMorph pages includes: aTool containerTab)
		ifTrue: [ tabMorph removePage: aTool containerTab ].
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
	self tabMorph page: existingTools first.
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
				ifTrue: [ extraTools := extraTools copyWith: mainTool ] ].
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
	
	tabMorph page: mainTool containerTab.
	
	^ mainTool
]

{ #category : #accessing }
ClyNotebookManager >> selectedTools [

	^ tabMorph page
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

{ #category : #private }
ClyNotebookManager >> tabOrderOf: anObject [

	^ anObject model 
		ifNotNil: [ :model | model tabOrder ] 
		ifNil: [ 0 ]
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

	self updateToolsBy: [ 
		self basicUpdateTools ]
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
