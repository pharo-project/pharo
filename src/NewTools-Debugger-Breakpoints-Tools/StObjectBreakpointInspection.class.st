"
I am an inspection for halts and breakpoints, my scope is the inspected object.
"
Class {
	#name : #StObjectBreakpointInspection,
	#superclass : #StInspection,
	#instVars : [
		'attributeTable',
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
StObjectBreakpointInspection >> breakPointsForModel [
	| methodsWithBreakpoint breakpoints |
	breakpoints := OrderedCollection new.
	methodsWithBreakpoint := model class allMethods
		select: [ :m | m hasBreakpoint ].
	breakpoints
		addAll:
			(methodsWithBreakpoint
				flatCollect: [ :m | 
					Breakpoint all
						select: [ :b | b isObjectCentric not and: [ b link methods includes: m compiledMethod] ] ]).
	breakpoints
		addAll:
			(Breakpoint all
				select:
					[ :b | b isObjectCentric and: [ b targetInstance == self model ] ]).
	breakpoints
		addAll:
			(Breakpoint all
				select: [ :b | 
					b isVariableBreakpoint
						and: [ b targetClassOrMethod == self model class
								and: [ b isObjectCentric not ] ] ]).
	^ breakpoints
]

{ #category : #private }
StObjectBreakpointInspection >> breakpointDataFor: aBreakpoint [
	"Returns a dictionary containing breakpoint data for the given breakpoint."

	^ aBreakpoint link nodes
		collect: [ :node | 
			| data |
			data := Dictionary new.
			data at: #node put: node.
			data at: #type put: 'Breakpoint'.
			data at: #interval put: (node start to: node stop).
			data at: #breakpoint put: aBreakpoint.
			data at: #target
				put:
					(aBreakpoint isObjectCentric
						ifTrue: [ 'self' ]
						ifFalse: [ node methodNode methodClass name ]).
			data ]
]

{ #category : #private }
StObjectBreakpointInspection >> buildBreakpointData [
	^ (self breakPointsForModel
		flatCollect: [ :b | self breakpointDataFor: b ])
		reject: [ :data | 
			| node breakpoint |
			node := data at: #node.
			breakpoint := data at: #breakpoint.
			node methodNode methodClass isAnonymous
				and: [ breakpoint isObjectCentric not
						or: [ breakpoint isVariableBreakpoint ] ] ]
]

{ #category : #private }
StObjectBreakpointInspection >> buildHaltAndBreakpointData [
	| breakpointData |
	breakpointData := SortedCollection
		sortBlock: [ :a :b | (a at: #node) methodNode selector < (b at: #node) methodNode selector ].
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
				evaluated: [ :item | self haltsAndBreakpoinsEnabledFor: item ])
				onActivation: [ :item | self enableHaltOrBreakpointFor: item ];
				onDeactivation: [ :item | self disableHaltOrBreakpointFor: item ];
				width: 20;
				yourself);
		addColumn:
			((SpStringTableColumn
				title: 'Type'
				evaluated: [ :item | item at: #type ])
				width: 65;
				yourself);
		addColumn:
			((SpStringTableColumn
				title: 'Target'
				evaluated: [ :item | item at: #target ])
				width: 120;
				yourself);
		addColumn:
			(SpStringTableColumn
				title: 'Method'
				evaluated: [ :item | 
					| node class name |
					node := item at: #node.
					class := node methodNode methodClass.
					name := class isAnonymous
						ifTrue: [ class superclass name ]
						ifFalse: [ class name ].
					name , '>>' , node methodNode selector ]).
	methodsWithBreakpoints
		contextMenu: self rootCommandsGroup asMenuPresenter
]

{ #category : #private }
StObjectBreakpointInspection >> buildHaltData [
	^ (model class allMethods
		flatCollect: [ :m | 
			StHaltCache defaultCache
				nodeCacheForSelector: m compiledMethod printString ])
		collect: [ :node | self haltDataFor: node ]
]

{ #category : #presenters }
StObjectBreakpointInspection >> buildSourceCode [
	"Source code of methods in which breakpoint are installed"

	breakpointsInSourceCode := self newCode.
	breakpointsInSourceCode disable
]

{ #category : #presenters }
StObjectBreakpointInspection >> buildTableData [
	methodsWithBreakpoints items: self buildHaltAndBreakpointData
]

{ #category : #presenters }
StObjectBreakpointInspection >> configureSourceCodeUpdate [

	methodsWithBreakpoints whenSelectionChangedDo: [ :selection | 
		selection isEmpty ifFalse: [ | selectedItem node |
			selectedItem := selection selectedItem.
			node := selectedItem at: #node.
			breakpointsInSourceCode text: node methodNode source.
			breakpointsInSourceCode selectionInterval: (selectedItem at: #interval).
			self updateSourceCodeBehaviorFor: node ] ]
]

{ #category : #breakpoints }
StObjectBreakpointInspection >> disableHaltOrBreakpointFor: breakpointData [
	(self isBreakpointObjectCentric: breakpointData)
		ifTrue: [ ^ breakpointController
				disableObjectCentricBreakpoint: (breakpointData at: #breakpoint) ].
	^ breakpointController
		disableHaltOrBreakpointFor: (breakpointData at: #node)
]

{ #category : #breakpoints }
StObjectBreakpointInspection >> enableHaltOrBreakpointFor: breakpointData [
	(self isBreakpointObjectCentric: breakpointData)
		ifTrue: [ ^ breakpointController
				enableObjectCentricBreakpoint: (breakpointData at: #breakpoint) ].
	^ breakpointController
		enableHaltOrBreakpointFor: (breakpointData at: #node)
]

{ #category : #private }
StObjectBreakpointInspection >> haltDataFor: aNode [
	"Returns a dictionary containing halt data for the given halt node."

	| data |
	data := Dictionary new.
	data at: #node put: aNode.
	data at: #type put: 'Halt'.
	data at: #interval put: (aNode start to: aNode stop).
	data at: #target put: aNode methodNode methodClass name.
	^ data
]

{ #category : #breakpoints }
StObjectBreakpointInspection >> haltsAndBreakpoinsEnabledFor: breakpointData [
	(self isBreakpointObjectCentric: breakpointData)
		ifTrue: [ ^ breakpointController
				isObjectCentricBreakpointEnabled: (breakpointData at: #breakpoint) ].
	^ breakpointController
		areHaltsAndBreakpoinsEnabledFor: (breakpointData at: #node)
]

{ #category : #private }
StObjectBreakpointInspection >> haltsSelectors [
	^#( halt halt: haltIf: haltIfNil haltOnCount: haltOnce)
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
	SystemAnnouncer uniqueInstance weak when: MetalinkChanged send: #buildTableData to: self
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

{ #category : #private }
StObjectBreakpointInspection >> objectCentricBreakpointsForModel [
	| methodsWithBreakpoint | 
	methodsWithBreakpoint := model class allMethods
		select: [ :m | m hasBreakpoint ].
	^ methodsWithBreakpoint
		flatCollect: [ :m | Breakpoint all select: [ :b | b link methods includes: m ] ]
]

{ #category : #breakpoints }
StObjectBreakpointInspection >> removeBreakpoint: aBreakpoint [
	aBreakpoint remove.
	self buildTableData 
]

{ #category : #'system subscription' }
StObjectBreakpointInspection >> unsubscribeFromCacheChanges [
	SystemAnnouncer uniqueInstance unsubscribe: self
]

{ #category : #presenters }
StObjectBreakpointInspection >> updateSourceCodeBehaviorFor: node [

	breakpointsInSourceCode beForBehavior: model class
]

{ #category : #private }
StObjectBreakpointInspection >> variableBreakpointsForModel [
	| methodsWithBreakpoint | 
	methodsWithBreakpoint := model class allMethods
		select: [ :m | m hasBreakpoint ].
	^ methodsWithBreakpoint
		flatCollect: [ :m | Breakpoint all select: [ :b | b link methods includes: m ] ]
]

{ #category : #updating }
StObjectBreakpointInspection >> windowIsClosing [
	^ self unsubscribeFromCacheChanges
]
