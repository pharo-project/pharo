"
I am a controller used by the ProfilerPresenter to interact with different kinds of profiler models
"
Class {
	#name : 'ProfilerModel',
	#superclass : 'MorphTreeModel',
	#instVars : [
		'profilerClass',
		'showLeavesOnly',
		'showNodeThreshold',
		'profiler',
		'tallyRoot',
		'isProfiling'
	],
	#category : 'ProfilerUI-Model',
	#package : 'ProfilerUI',
	#tag : 'Model'
}

{ #category : 'constants' }
ProfilerModel >> allProfilerClasses [

	^ { AndreasSystemProfiler . TimeProfiler ". PreciseProfiler" }
]

{ #category : 'announce' }
ProfilerModel >> announceProfilingFinished [

	^ self announcer announce: ProfilingFinishedAnnouncement new
]

{ #category : 'announce' }
ProfilerModel >> announceProfilingStarted [
	self announcer announce: ProfilingStartedAnnouncement new
]

{ #category : 'accessing nodes' }
ProfilerModel >> announceRootChanged: aTallyModel [

	self announcer announce:
		(ProfilerResultsTreeRootChangedAnnouncement new item: aTallyModel)
]

{ #category : 'announce' }
ProfilerModel >> announceSelectionChanged: aTallyModel [

	self announcer announce:
		(ProfilerResultsTreeSelectionChangedAnnouncement new item:
			 aTallyModel)
]

{ #category : 'event handling' }
ProfilerModel >> browseItem [
	self selectedNode ifNotNil: [:current | current browseItem ]
]

{ #category : 'constants' }
ProfilerModel >> doItSelectorsPath [

	^ profiler doItSelectorsPath
]

{ #category : 'constants' }
ProfilerModel >> doItSourceCodeFrom: someCode [
	^ 'self profileBlock: [',(profiler blockCodeFrom: someCode),' ]'
]

{ #category : 'api' }
ProfilerModel >> hideNodesUnder: aNumber [
	"minimum percentage a node must have to be shown"
	showNodeThreshold := aNumber
]

{ #category : 'initialization' }
ProfilerModel >> initialize [
	super initialize.
	showLeavesOnly := false.
	showNodeThreshold := 0
]

{ #category : 'accessing' }
ProfilerModel >> isProfiling [
	^ isProfiling ifNil: [ isProfiling := false ]
]

{ #category : 'event handling' }
ProfilerModel >> keyDown: aKeyboardEvent from: aMorphTreeListManager [
	self selectedNode ifNotNil: [:current | current keyDown: aKeyboardEvent from: aMorphTreeListManager ]
]

{ #category : 'accessing nodes' }
ProfilerModel >> leaveItems [
	| res  |
	res := OrderedCollection new.
	self tallyRoot
		ifNotNil: [
			| dict |
			dict := IdentityDictionary new: 100.
			self tallyRoot leavesInto: dict fromSender: nil.
			res addAll: (dict asOrderedCollection sort reverse)].
	^ res
]

{ #category : 'accessing nodes' }
ProfilerModel >> leaveItemsOver: perCent [

	| leaveThreshold |
	self tallyRoot ifNil: [ ^ #() ].
	leaveThreshold := (perCent asFloat / 100 * self tallyRoot tally) rounded.
	^ self leaveItems select: [ :item | item tally > leaveThreshold ]
]

{ #category : 'accessing' }
ProfilerModel >> menu: menu shifted: b [
	"Set up the menu to apply to the receiver's, honoring the #shifted boolean"

	self selectedNode
		ifNotNil: [ :current |
			current menu: menu shifted: b.
			menu addLine.
			(menu
				add: 'Expand all from here'
				target: self
				selector: #expandAllFromNode:
				argument: current) icon: (self iconNamed: #windowMenuInactive).
			menu addLine.
			(menu
				add: 'Show as root'
				target: self
				selector: #setTallyRoot:
				argument: current)
				icon: (self iconNamed: #glamorousRedo) ].
	^ menu
]

{ #category : 'factory' }
ProfilerModel >> newProfiler [
	^ self profilerClass new
]

{ #category : 'private' }
ProfilerModel >> profileBlock: aBlock [

	"Fork is used to get a more clean execution results tree"
	[self announceProfilingStarted.
	self startProfiling.
	aBlock ensure: [
		self stopProfiling.
		self announceProfilingFinished ]] fork
]

{ #category : 'api' }
ProfilerModel >> profileCode: someCode notifying: requestor [

	| compiledMethod |
	self isProfiling ifTrue: [ profiler stopProfiling ].
	profiler := self newProfiler.

	compiledMethod := Smalltalk compiler
		                  source: (self doItSourceCodeFrom: someCode);
		                  requestor: requestor;
		                  isScripting: true;
		                  failBlock: [ ^ self ];
		                  compile.
	compiledMethod valueWithReceiver: self arguments: #(  )
]

{ #category : 'accessing' }
ProfilerModel >> profilerClass [
	^ profilerClass ifNil: [ profilerClass := self allProfilerClasses at: 1 ]
]

{ #category : 'accessing' }
ProfilerModel >> profilerClass: aClass [
	profilerClass := aClass
]

{ #category : 'reporting' }
ProfilerModel >> report [

	^ profiler report
]

{ #category : 'accessing' }
ProfilerModel >> rootItem [
	^ self tallyRoot
]

{ #category : 'accessing' }
ProfilerModel >> rootItems [
	self tallyRoot ifNil: [ ^#() ].

	^ showLeavesOnly
		  ifTrue: [ self leaveItemsOver: showNodeThreshold ]
		  ifFalse: [ {self tallyRoot asTallyModel
			  							model: self;
			  							yourself} ]
]

{ #category : 'accessing' }
ProfilerModel >> rootNodeFromItem: anItem [

	^ anItem asTallyModel
]

{ #category : 'accessing' }
ProfilerModel >> rootNodes [

	profiler ifNil: [ ^ nil ].
	^ super rootNodes
]

{ #category : 'accessing nodes' }
ProfilerModel >> rootTallyItems [

	^ self rootTallyItemsOver: 0
]

{ #category : 'accessing nodes' }
ProfilerModel >> rootTallyItemsOver: perCent [
	| minTally |
	self tallyRoot ifNil: [ ^ #() ].
	minTally := (perCent asFloat / 100 * self tallyRoot tally) rounded.

	^ (self tallyRoot sonsOver: minTally)
		ifEmpty: [ #() ]
		ifNotEmpty: [:sons | sons asOrderedCollection sort reverse ]
]

{ #category : 'private - running' }
ProfilerModel >> runBlock: aBlock [

	^ profiler runBlock: aBlock
]

{ #category : 'accessing nodes' }
ProfilerModel >> setTallyRoot: aTallyModel [

	tallyRoot = aTallyModel ifTrue: [ ^ self ].
	tallyRoot := aTallyModel.
	self announceRootChanged: tallyRoot
]

{ #category : 'accessing' }
ProfilerModel >> showLeavesOnly [

	^ showLeavesOnly
]

{ #category : 'accessing' }
ProfilerModel >> showLeavesOnly: anObject [

	showLeavesOnly := anObject.
	self announceRootChanged: tallyRoot
]

{ #category : 'accessing' }
ProfilerModel >> showNodeThreshold [
	^ showNodeThreshold
]

{ #category : 'profiling' }
ProfilerModel >> startProfiling [

	isProfiling := true.
	tallyRoot := nil.
	profiler startProfiling
]

{ #category : 'profiling' }
ProfilerModel >> stopProfiling [

	profiler stopProfiling.
	self setTallyRoot: (profiler tallyRoot asTallyModel
		             model: self;
		             yourself).
	isProfiling := false
]

{ #category : 'accessing nodes' }
ProfilerModel >> tallyRoot [
	^ tallyRoot
]

{ #category : 'accessing' }
ProfilerModel >> total [
	^ self tallyRoot tally
]

{ #category : 'accessing' }
ProfilerModel >> totalTally [
	^ self tallyRoot tally
]

{ #category : 'accessing' }
ProfilerModel >> totalTime [
	^ profiler totalTime
]
