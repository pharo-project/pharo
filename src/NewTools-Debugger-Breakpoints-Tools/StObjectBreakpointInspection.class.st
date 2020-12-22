"
I am an inspection for halts and breakpoints, my scope is the inspected object.
"
Class {
	#name : #StObjectBreakpointInspection,
	#superclass : #StInspection,
	#instVars : [
		'methodsWithBreakpoints',
		'breakpointsInSourceCode',
		'breakpointController'
	],
	#category : #'NewTools-Debugger-Breakpoints-Tools-Presenters'
}

{ #category : #specs }
StObjectBreakpointInspection class >> defaultSpec [

	^ SpPanedLayout newVertical
		add: #methodsWithBreakpoints;
		add: #breakpointsInSourceCode;
		yourself
]

{ #category : #private }
StObjectBreakpointInspection >> breakpointsForModel [

	^ Breakpoint all select: [ :bp | bp scopeIncludes: model ]
]

{ #category : #private }
StObjectBreakpointInspection >> buildBreakpointData [
	^ (self breakpointsForModel
		collect: [ :b | StBreakpointInspectionItem forBreakpoint: b ])
	"	reject: [ :data | 
			| node breakpoint |
			node := data at: #node.
			breakpoint := data at: #breakpoint.
			node methodNode methodClass isAnonymous
				and: [ breakpoint isObjectCentric not
						or: [ breakpoint isVariableBreakpoint ] ] ]"
]

{ #category : #private }
StObjectBreakpointInspection >> buildHaltAndBreakpointData [
	| breakpointData |
	breakpointData := OrderedCollection new."SortedCollection
		sortBlock: [ :a :b | (a at: #node) methodNode selector < (b at: #node) methodNode selector ]."
	breakpointData addAll: self buildBreakpointData.
	breakpointData addAll: self buildHaltData.
	^ breakpointData
]

{ #category : #presenters }
StObjectBreakpointInspection >> buildHaltAndBreakpointTable [
	"List of methods in which a breakpoint is installed"

	methodsWithBreakpoints := self newTable.
	methodsWithBreakpoints 
		addColumn:
			((SpCheckBoxTableColumn
				title: ' '
				evaluated: [ :item | 	self haltOrBreakpoinsEnabledFor: item ])
				onActivation: [ :item | self enableHaltOrBreakpointFor: item ];
				onDeactivation: [ :item | self disableHaltOrBreakpointFor: item ];
				width: 20;
				yourself);
		addColumn:
			((SpStringTableColumn
				title: 'Type'
				evaluated: [ :item | item type ])
				width: 65;
				yourself);
		addColumn:
			((SpStringTableColumn
				title: 'Scope'
				evaluated: [ :item | item printScope])
				width: 120;
				yourself);
		addColumn:
			(SpStringTableColumn
				title: 'Target'
				evaluated: [ :item | item printEntity]).
	methodsWithBreakpoints
		contextMenu: self rootCommandsGroup asMenuPresenter
]

{ #category : #private }
StObjectBreakpointInspection >> buildHaltData [

	^ (model class allMethods flatCollect: [ :m | 
		   StHaltCache defaultCache nodeCacheForSelector:
			   m compiledMethod printString ]) collect: [ :node | 
		  StHaltInspectionItem forHalt: node ]
]

{ #category : #presenters }
StObjectBreakpointInspection >> buildSourceCode [
	"Source code of methods in which breakpoint are installed"

	breakpointsInSourceCode := self newCode.
	breakpointsInSourceCode beNotEditable
]

{ #category : #presenters }
StObjectBreakpointInspection >> buildTableData [
	methodsWithBreakpoints items: self buildHaltAndBreakpointData
]

{ #category : #presenters }
StObjectBreakpointInspection >> configureSourceCodeUpdate [

	methodsWithBreakpoints whenSelectionChangedDo: [ :selection | 
		selection isEmpty ifFalse: [ 
			| selectedItem |
			selectedItem := selection selectedItem.
			breakpointsInSourceCode text: selectedItem printContents.
			selectedItem updateSourceCodePaneFor: self ] ]
]

{ #category : #breakpoints }
StObjectBreakpointInspection >> disableHaltOrBreakpointFor: haltOrBreakpointItem [

	breakpointController disableHaltOrBreakpointFor: haltOrBreakpointItem
]

{ #category : #commands }
StObjectBreakpointInspection >> doBrowseBreakpointMethod [
	| entityToBrowse |
	
	entityToBrowse := self methodsWithBreakpoints selection selectedItem entityToBrowse.
	self systemNavigation browse: entityToBrowse.
]

{ #category : #breakpoints }
StObjectBreakpointInspection >> enableHaltOrBreakpointFor: haltOrBreakpointItem [

	breakpointController enableHaltOrBreakpointFor: haltOrBreakpointItem
]

{ #category : #breakpoints }
StObjectBreakpointInspection >> haltOrBreakpoinsEnabledFor: haltOrBreakpointItem [

	^ breakpointController isHaltOrBreakpointEnabled:
		  haltOrBreakpointItem
]

{ #category : #initialization }
StObjectBreakpointInspection >> initialize [
	breakpointController := StHaltAndBreakpointController new.
	super initialize
]

{ #category : #initialization }
StObjectBreakpointInspection >> initializePresenters [
	self buildSourceCode.
	self buildHaltAndBreakpointTable.
	self configureSourceCodeUpdate.
	self buildTableData.
	SystemAnnouncer uniqueInstance weak when: StHaltCacheChanged send: #buildTableData to: self.
	SystemAnnouncer uniqueInstance weak when: BreakpointAdded send: #buildTableData to: self
]

{ #category : #initialization }
StObjectBreakpointInspection >> initializeWindow: aWindowPresenter [
	super initializeWindow: aWindowPresenter.
	aWindowPresenter
		whenClosedDo: [ SystemAnnouncer uniqueInstance unsubscribe: self ]
]

{ #category : #breakpoints }
StObjectBreakpointInspection >> isBreakpointObjectCentric: breakpointData [
	| breakpoint |
	breakpoint := breakpointData at: #breakpoint ifAbsent: [ ^ false ].
	^ breakpoint isObjectCentric
		and: [ breakpoint isVariableBreakpoint not ]
]

{ #category : #presenters }
StObjectBreakpointInspection >> labelForHaltSelector: aSelector [
	"This was intended to be used to display more precise information about the kind of halt we display in the presenter."
	self flag: 'Not used yet. Kept as documentation for further use'.
	(#(haltIf: haltIfNil) includes: aSelector)
		ifTrue: [ ^ 'condition' ].
	(#(haltOnCount:) includes: aSelector)
		ifTrue: [ ^ 'on count' ].
	(#(haltOnce) includes: aSelector)
		ifTrue: [ ^ 'once' ].
	^ 'always'
]

{ #category : #accessing }
StObjectBreakpointInspection >> methodsWithBreakpoints [
	^ methodsWithBreakpoints
]

{ #category : #'system subscription' }
StObjectBreakpointInspection >> unsubscribeFromCacheChanges [
	SystemAnnouncer uniqueInstance unsubscribe: self
]

{ #category : #updating }
StObjectBreakpointInspection >> updateSourceCodeForBreakpointItem: aStBreakpointInspectionItem [

	"Halts and standard breakpoints items both behave the same"

	self updateSourceCodeForHaltItem: aStBreakpointInspectionItem
]

{ #category : #updating }
StObjectBreakpointInspection >> updateSourceCodeForHaltItem: aStHaltInspectionItem [

	breakpointsInSourceCode withLineNumbers.
	breakpointsInSourceCode selectionInterval:
		aStHaltInspectionItem interval.
	breakpointsInSourceCode beForBehavior: model class
]

{ #category : #updating }
StObjectBreakpointInspection >> updateSourceCodeForVariableBreakpointItem: aStBreakpointInspectionItem [

	breakpointsInSourceCode
		beForObject: model;
		withoutLineNumbers.
	breakpointsInSourceCode beForBehavior: model class
]

{ #category : #updating }
StObjectBreakpointInspection >> windowIsClosing [
	^ self unsubscribeFromCacheChanges
]
