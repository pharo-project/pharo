"
TimeProfiler is a graphical front end of MessageTally. It gives nice facilities to navigate and browse a profile.

Use examples:
TimeProfiler new open
TimeProfiler new openOnBlock: [SimpleGridExample new open]
TimeProfiler new openOnBlock: [ClassTreeExample new openOn: Object]
 MessageTally spyAllOn: [ClassTreeExample new openOn: Object]
"
Class {
	#name : 'TimeProfiler',
	#superclass : 'Object',
	#instVars : [
		'rootMessageTally',
		'threshold',
		'block',
		'reportOtherProcesses',
		'showLeavesOnly',
		'result'
	],
	#category : 'Tool-Profilers-Time',
	#package : 'Tool-Profilers',
	#tag : 'Time'
}

{ #category : 'examples' }
TimeProfiler class >> example [

	[1000 timesRepeat: [3.14159 printString. Processor yield]] fork.
	[1000 timesRepeat: [20 factorial. Processor yield]] fork.
	[1000 timesRepeat: [20 factorial. Processor yield]] fork.
	self spyAllOn: [ (Delay forMilliseconds: 100) wait]
]

{ #category : 'examples' }
TimeProfiler class >> exampleOnBlock [

	self onBlock: [ 20 timesRepeat: [self trace: 100 factorial printString]]
]

{ #category : 'opening' }
TimeProfiler class >> onBlock: block [
	"Open a profile browser on the given block, thereby running the block and
	 collecting the message tally."

	| inst result |
	inst := self new.
	result := inst runBlock: block.
	inst open.
	^ result
]

{ #category : 'opening' }
TimeProfiler class >> spyAllOn: aBlock [
	"Spy on all the processes in the system"

	| inst result |
	inst := self new.
	result := inst spyAllOn: aBlock.
	inst open.
	^ result
]

{ #category : 'opening' }
TimeProfiler class >> spyFor: seconds [
	"Run the system profiler for the specified number of seconds"

	^self spyOn: [ (Delay forSeconds: seconds) wait ]
]

{ #category : 'opening' }
TimeProfiler class >> spyOn: aBlock [

	^ self new openOnBlock: aBlock
]

{ #category : 'opening' }
TimeProfiler class >> spyOnProcess: aProcess forMilliseconds: msecDuration [
	"Run aProcess for msecDuration milliseconds, then open a TimeProfiler on the results."

	| inst |
	inst := self new.
	inst runProcess: aProcess forMilliseconds: msecDuration pollingEvery: MessageTally defaultPollPeriod.
	inst open.
	^ inst
]

{ #category : 'icons' }
TimeProfiler class >> taskbarIconName [
	^#smallProfile
]

{ #category : 'accessing' }
TimeProfiler >> fullReport [
	"Return the full report of the tally "

	^ String streamContents: [ :strm | rootMessageTally ifNotNil: [ rootMessageTally report: strm ] ]
]

{ #category : 'accessing' }
TimeProfiler >> leaveItems [

	| res |
	res := OrderedCollection new.
	rootMessageTally ifNotNil: [
			| dict |
			dict := IdentityDictionary new: 100.
			rootMessageTally leavesInto: dict fromSender: nil.
			res addAll: (dict asOrderedCollection select: [ :node | node tally > self threshold ]) ].
	^ res
]

{ #category : 'actions' }
TimeProfiler >> openOnBlock: aBlock [

	self runBlock: aBlock pollingEvery: MessageTally defaultPollPeriod.
	^ self open
]

{ #category : 'accessing' }
TimeProfiler >> reportOtherProcesses [
	^ reportOtherProcesses ifNil: [reportOtherProcesses := true]
]

{ #category : 'accessing' }
TimeProfiler >> reportOtherProcesses: aBoolean [

	reportOtherProcesses := aBoolean
]

{ #category : 'accessing' }
TimeProfiler >> result: anObject [
	result := anObject
]

{ #category : 'accessing' }
TimeProfiler >> rootItems [
	^ rootMessageTally
		ifNil: [#()]
		ifNotNil: [
			self showLeavesOnly
				ifTrue: [ self leaveItems ]
				ifFalse: [ self rootTallyItems]]
]

{ #category : 'accessing' }
TimeProfiler >> rootTallyItems [

	| res sons groups |
	res := OrderedCollection new.
	rootMessageTally ifNotNil: [
			sons := rootMessageTally sonsOver: self threshold.
			groups := sons groupedBy: [ :aTally | aTally process ] having: [ :g | true ].
			groups do: [ :g |
					self reportOtherProcesses
						ifTrue: [ res addAll: g ]
						ifFalse: [ res addAll: (g asSortedCollection reject: [ :gg | gg process isNil ]) ] ] ].
	^ res
]

{ #category : 'actions' }
TimeProfiler >> runBlock: aBlock [
	^ self runBlock: aBlock pollingEvery: MessageTally defaultPollPeriod
]

{ #category : 'actions' }
TimeProfiler >> runBlock: aBlock pollingEvery: pollPeriod [
	block := aBlock.
	rootMessageTally := MessageTally new.
	rootMessageTally
		reportOtherProcesses: true;
		maxClassNameSize: 1000;
		maxClassPlusSelectorSize: 1000;
		maxTabs: 100.
	result := rootMessageTally spyEvery: pollPeriod on: aBlock.
	self result: result.
	^ result
]

{ #category : 'actions' }
TimeProfiler >> runProcess: aProcess forMilliseconds: msecDuration pollingEvery: pollPeriod [
	block := MessageSend
				receiver: self
				selector: #runProcess:forMilliseconds:pollingEvery:
				arguments: {
						aProcess.
						msecDuration.
						pollPeriod}.	"so we can re-run it"
	rootMessageTally := MessageTally new.
	rootMessageTally
		reportOtherProcesses: false;
		maxClassNameSize: 1000;
		maxClassPlusSelectorSize: 1000;
		maxTabs: 100.
	result := rootMessageTally
				spyEvery: pollPeriod
				onProcess: aProcess
				forMilliseconds: msecDuration.
	^result
]

{ #category : 'accessing' }
TimeProfiler >> selectedClass [
	^ self selectedNode
		ifNotNil: [ :currNode | currNode methodClass ]
]

{ #category : 'accessing' }
TimeProfiler >> showLeavesOnly [

	^ showLeavesOnly ifNil: [ showLeavesOnly := false ]
]

{ #category : 'accessing' }
TimeProfiler >> showLeavesOnly: aBoolean [

	showLeavesOnly := aBoolean
]

{ #category : 'actions' }
TimeProfiler >> spyAllOn: aBlock [
	block := aBlock.
	rootMessageTally := MessageTally new.
	rootMessageTally
		reportOtherProcesses: true;
		maxClassNameSize: 1000;
		maxClassPlusSelectorSize: 1000;
		maxTabs: 100.
	result := rootMessageTally spyAllEvery: MessageTally defaultPollPeriod on: aBlock.
	self result: result.
	^ result
]

{ #category : 'accessing' }
TimeProfiler >> tally [
	^ rootMessageTally tally
]

{ #category : 'accessing' }
TimeProfiler >> threshold [

	^ threshold ifNil: [ threshold := 0 ]
]

{ #category : 'accessing' }
TimeProfiler >> threshold: aTreshold [

	(aTreshold between: 0 and: 1000) ifFalse: [ ^ self ].

	threshold := aTreshold
]

{ #category : 'accessing' }
TimeProfiler >> time [
	^ rootMessageTally time
]
