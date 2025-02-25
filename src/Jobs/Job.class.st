"
A Job is a task to run and potentially notified to the user.

[:job | job title: 'Let us get started'.
	1 to: 10 do: [:each |
		job
			progress: (0.1 * each);
			title: 'Youpi ', each printString.
		(Delay forMilliseconds: 100) wait.
		] ]  asJob run
"
Class {
	#name : 'Job',
	#superclass : 'Object',
	#instVars : [
		'block',
		'currentValue',
		'min',
		'max',
		'title',
		'children',
		'isRunning',
		'parent',
		'process',
		'owner',
		'announcer',
		'interruptBlock'
	],
	#classInstVars : [
		'jobAnnouncer'
	],
	#category : 'Jobs-Base',
	#package : 'Jobs',
	#tag : 'Base'
}

{ #category : 'instance creation' }
Job class >> block: aBlock [

	^(self new)
		block: aBlock;
		yourself
]

{ #category : 'instance creation' }
Job class >> block: aBlock owner: anObject [

	^(self new)
		block: aBlock;
		owner: anObject;
		yourself
]

{ #category : 'accessing' }
Job class >> current [
	"Answer the current job or nil if none."

	^ CurrentJob value
]

{ #category : 'examples' }
Job class >> exampleCurrentValue [

	[ :job| 
		job title: 'Simulating some progress for 1 Second'. 
		1 second wait. "simulate some work"
		job currentValue: 50.
		1 second wait. "simulate some more work"
		job currentValue: 100.
		1 second wait. "simulate some more work"
	] asJob run
]

{ #category : 'examples' }
Job class >> exampleDebug [
	"Job exampleDebug"
	| aJob |
	aJob := [ :job| 
		"Set some job properties"
		job 
			title: 'aTitle';
			max: 10.
		1 to: 10 do: [ :i|
			job title: 'Fib ', i asString.
			"do some hard work"
			40 benchFib.
			"update the job progress"
			job currentValue: i. ]
	] asJob.
	
	"run a different thread to interrupt the job"
	[ aJob run ] forkAt: Processor userBackgroundPriority.
	(Delay forSeconds: 1) wait. "wait for the job to start properly"
	aJob debug
]

{ #category : 'examples' }
Job class >> exampleProgress [

	[:job | job title: 'Let us get started'.
		1 to: 10 do: [:each | 
			job 
				progress: (0.1 * each); 
				title: 'Youpi ', each printString.
			(Delay forMilliseconds: 100) wait. 
			] ]  asJob run
]

{ #category : 'announcing' }
Job class >> jobAnnouncer [
	"Answers the announcer for job announcements"
	
	^ jobAnnouncer ifNil: [ jobAnnouncer := Announcer new ]
]

{ #category : 'private' }
Job >> addChild: aJob [

	children add: aJob.
	aJob parent: self
]

{ #category : 'private - announcing' }
Job >> announce: anAnnouncementClass [

	| announcement |
	announcement := anAnnouncementClass on: self.
	self announcer announce: announcement
]

{ #category : 'private - announcing' }
Job >> announceChange [

	isRunning ifFalse: [ ^ self ].
	self announce: JobChange
]

{ #category : 'private - announcing' }
Job >> announceEnd [

	self announce: JobEnd
]

{ #category : 'private - announcing' }
Job >> announceStart [

	self announce: JobStart
]

{ #category : 'private - announcing' }
Job >> announcer [

	^ announcer ifNil: [ self class jobAnnouncer ]
]

{ #category : 'accessing' }
Job >> block [
	
	^ block
]

{ #category : 'private' }
Job >> block: aBlock [
	
	block := aBlock
]

{ #category : 'accessing' }
Job >> children [

	^ children copy
]

{ #category : 'private' }
Job >> cleanupAfterRunning [

	isRunning := false.
	process := nil.
	self announceEnd.
	parent ifNotNil: [ :job | 
		job removeChild: self ]
]

{ #category : 'compatibility' }
Job >> current [

	^ self currentValue
]

{ #category : 'compatibility' }
Job >> current: aNumber [

	self currentValue: aNumber
]

{ #category : 'accessing' }
Job >> currentValue [
	
	^ currentValue
]

{ #category : 'accessing' }
Job >> currentValue: aNumber [
	
	currentValue := aNumber.
	self announceChange
]

{ #category : 'debugging' }
Job >> debug [
	^ process debug
]

{ #category : 'compatibility' }
Job >> decrement [

	self currentValue: self currentValue - 1
]

{ #category : 'compatibility' }
Job >> increment [

	self currentValue: self currentValue + 1.
]

{ #category : 'initialization' }
Job >> initialize [

	super initialize.
	min := 0.
	max := 100.
	currentValue := 0.
	title := ''.
	isRunning := false.
	children := OrderedCollection new.
	interruptBlock := [ process debug ]
]

{ #category : 'accessing' }
Job >> installOn: aPresenter [
	"this method is provider for compatibility"
	
	announcer := aPresenter announcer
]

{ #category : 'running' }
Job >> interrupt [

	self isRunning ifTrue: [ interruptBlock cull: self ]
]

{ #category : 'accessing' }
Job >> interruptBlock [

	^ interruptBlock
]

{ #category : 'accessing' }
Job >> interruptBlock: aBlock [

	interruptBlock := aBlock
]

{ #category : 'testing' }
Job >> isRunning [
	^ isRunning
]

{ #category : 'compatibility' }
Job >> label [

	^ self title
]

{ #category : 'compatibility' }
Job >> label: aString [ 

	self title: aString.
]

{ #category : 'accessing' }
Job >> lookup: lookupBlock ifNone: noneBlock [
	"Detect a job that satisfies the lookupBlock, or value noneBlock if none satisfies. 
	The lookup starts at myself, following recursevely through my parent."

	(lookupBlock value: self) ifTrue: [ ^ self ].
		
	^ parent 
		ifNil: noneBlock 
		ifNotNil: [ parent lookup: lookupBlock ifNone: noneBlock ]
]

{ #category : 'accessing' }
Job >> max [
	
	^ max
]

{ #category : 'accessing' }
Job >> max: aNumber [
	self migrateProgressWhileUpdatingBounds: [ max := aNumber ].
]

{ #category : 'accessing' }
Job >> migrateProgressWhileUpdatingBounds: aBlockChangingBounds [
	"Keep the progress value consistent while we change min / max"
	| progress |
	progress := self progress.
	aBlockChangingBounds value.
	self progress: progress.
]

{ #category : 'accessing' }
Job >> min [
	
	^ min
]

{ #category : 'accessing' }
Job >> min: aNumber [
	self migrateProgressWhileUpdatingBounds: [ min := aNumber ].
	self announceChange.
]

{ #category : 'accessing' }
Job >> owner [
	^ owner
]

{ #category : 'accessing' }
Job >> owner: anObject [ 
	owner := anObject
]

{ #category : 'private' }
Job >> parent [

	^ parent
]

{ #category : 'private' }
Job >> parent: aJob [ 

	parent := aJob.
]

{ #category : 'private' }
Job >> prepareForRunning [

	isRunning := true.
	process := Processor activeProcess.
	CurrentJob value ifNotNil: [ :job | job addChild: self].
	self announceStart
]

{ #category : 'progress' }
Job >> progress [
	"Avoid negative progress and divideByZero."
	^ min >= max ifTrue: [ 1 ] ifFalse: [ (currentValue - min) / (max - min) ]
]

{ #category : 'progress' }
Job >> progress: aNormalizedFloat [
	"Set the progress: 0.0 - 1.0"

	currentValue := (min + ((max - min) * aNormalizedFloat)).
	self announceChange.
]

{ #category : 'private' }
Job >> removeChild: aJob [ 

	children remove: aJob.
]

{ #category : 'running' }
Job >> run [

	[ self prepareForRunning.
	  CurrentJob 
		value: self 
		during: [ ^ block cull: self ] ]
	ensure: [ 
		self cleanupAfterRunning ]
]

{ #category : 'running' }
Job >> terminate [

	process terminate
]

{ #category : 'accessing' }
Job >> title [
	
	^ title
]

{ #category : 'accessing' }
Job >> title: anObject [
	
	title := anObject.
	self announceChange.
]

{ #category : 'compatibility' }
Job >> value [

	^ self currentValue.
]

{ #category : 'compatibility' }
Job >> value: aNumber [

	self currentValue: aNumber.
]
