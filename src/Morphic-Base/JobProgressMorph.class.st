"
I combine a progress title and a JobPorgressBarMorph to a visual representation of a Job.

Usually I am contained in the global instance of the SystemProgressMorph
"
Class {
	#name : 'JobProgressMorph',
	#superclass : 'Morph',
	#instVars : [
		'bar',
		'endValue',
		'hasResult',
		'job',
		'labelMorph',
		'lastRefresh',
		'lock',
		'result',
		'startValue',
		'pendingLabel',
		'labelUpdateScheduled'
	],
	#category : 'Morphic-Base-ProgressBar',
	#package : 'Morphic-Base',
	#tag : 'ProgressBar'
}

{ #category : 'instance creation' }
JobProgressMorph class >> job: aJob [
	^ self new initializeJob: aJob
]

{ #category : 'api' }
JobProgressMorph >> beComplete [

	self close
]

{ #category : 'updating' }
JobProgressMorph >> changed [

	(self isInWorld and: [ self isTimeForRefresh ]) ifFalse: [ ^ self ].
	lastRefresh := Time millisecondClockValue.
	super changed
]

{ #category : 'private' }
JobProgressMorph >> close [

	SystemProgressMorph uniqueInstance close: self.
	self removeDependent: SystemProgressMorph uniqueInstance
]

{ #category : 'api' }
JobProgressMorph >> current [

	^ bar value
]

{ #category : 'api' }
JobProgressMorph >> current: aNumber [

	bar value: aNumber.
	self changed
]

{ #category : 'api' }
JobProgressMorph >> decrement [

	bar decrement
]

{ #category : 'private' }
JobProgressMorph >> do: aBlock [

	self changed. "We may be in the UI thread, so this will give us a change to dsiplay ourselves before the block starts"
	^ [ aBlock value: self ] ensure: [ self close ]
]

{ #category : 'private' }
JobProgressMorph >> forceRefreshOnNextChange [

	lastRefresh := 0
]

{ #category : 'api' }
JobProgressMorph >> increment [

	bar increment
]

{ #category : 'initialization' }
JobProgressMorph >> initialize [

	super initialize.
	self addDependent: SystemProgressMorph uniqueInstance
]

{ #category : 'private' }
JobProgressMorph >> initializeJob: aJob [

	super initialize.

	lock := Semaphore forMutualExclusion.
	job := aJob.
	hasResult := false.
	lastRefresh := 0.
	labelUpdateScheduled := false.

	self
		color: Color transparent;
		layoutPolicy: TableLayout new;
		listDirection: #topToBottom;
		cellPositioning: #leftCenter;
		listCentering: #center;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap.

	labelMorph := StringMorph
		contents: job title
		font: StandardFonts defaultFont.
	bar := JobProgressBarMorph new.
	bar
		on: #mouseUp send: #interrupt to: aJob;
		hResizing: #spaceFill.
	self updateLayout
]

{ #category : 'testing' }
JobProgressMorph >> isTimeForRefresh [

	"Answer true when enough time elapsed since the last update to update the progress again."
	^ Time millisecondClockValue - self lastRefresh >= 60 "roughly 16 times per second"
]

{ #category : 'accessing' }
JobProgressMorph >> job: aJob [

	job := aJob
]

{ #category : 'api' }
JobProgressMorph >> label [

	^ self labelMorph contents
]

{ #category : 'api' }
JobProgressMorph >> label: aString [

	self label = aString ifTrue: [ ^ self ].

	self label ifEmpty: [
			aString isEmptyOrNil ifTrue: [ ^ self ].
			self removeAllMorphs.
			self labelMorph contents: aString.
			self updateLayout ].

	aString isEmptyOrNil
		ifTrue: [ self removeMorph: self labelMorph ]
		ifFalse: [ self labelMorph contents: aString ].

	self changed: #width
]

{ #category : 'private' }
JobProgressMorph >> labelMorph [

	^ labelMorph
]

{ #category : 'accessing' }
JobProgressMorph >> lastRefresh [

	^ lastRefresh ifNil: [ lastRefresh := 0 ]
]

{ #category : 'private' }
JobProgressMorph >> max: aNumber [

	endValue := aNumber
]

{ #category : 'api' }
JobProgressMorph >> progress [

	^ bar progress
]

{ #category : 'api' }
JobProgressMorph >> progress: aNormalizedNumber [

	bar progress = aNormalizedNumber ifFalse: [
			bar progress: aNormalizedNumber.
			self changed ]
]

{ #category : 'api' }
JobProgressMorph >> progress: aNormalizedNumber label: aString [

	"I am here to have a version updating only once the progress bar when we change label and progress. The label may be refreshed later, so the last requested label is never dropped."

	| changed |
	changed := false.
	bar progress = aNormalizedNumber ifFalse: [
			bar progress: aNormalizedNumber.
			changed := true ].

	pendingLabel := aString.
	self label = aString ifTrue: [
			changed ifTrue: [ self changed ].
			^ self ].

	self updateLabelToPending
]

{ #category : 'private' }
JobProgressMorph >> refresh [

	lastRefresh := Time millisecondClockValue.
	self width: labelMorph width + 25
]

{ #category : 'private' }
JobProgressMorph >> result [

	^ result
]

{ #category : 'private' }
JobProgressMorph >> result: anObject [

	lock critical: [
		hasResult := true.
		result := anObject ]
]

{ #category : 'private' }
JobProgressMorph >> scheduleLabelUpdate [

	"Retry the label update once the refresh delay elapsed, from the UI process."

	labelUpdateScheduled ifTrue: [ ^ self ].
	labelUpdateScheduled := true.
	[ (Delay forMilliseconds: 60) wait.
	UIManager default defer: [
			labelUpdateScheduled := false.
			self updateLabelToPending ] ] forkAt: Processor userBackgroundPriority
]

{ #category : 'private' }
JobProgressMorph >> start: aNumber [

	startValue := aNumber
]

{ #category : 'private' }
JobProgressMorph >> startAt: aNumber [

	startValue := aNumber
]

{ #category : 'private' }
JobProgressMorph >> updateLabelToPending [

	"Apply the last requested label once the refresh delay elapsed, otherwise schedule it."

	self label = pendingLabel ifTrue: [ ^ self ].
	self isTimeForRefresh ifFalse: [ ^ self scheduleLabelUpdate ].

	lastRefresh := Time millisecondClockValue.
	self label ifEmpty: [
			pendingLabel isEmptyOrNil ifTrue: [ ^ self ].
			self removeAllMorphs.
			self labelMorph contents: pendingLabel.
			self updateLayout ].

	pendingLabel isEmptyOrNil
		ifTrue: [ self removeMorph: self labelMorph ]
		ifFalse: [ self labelMorph contents: pendingLabel ].

	self changed: #width
]

{ #category : 'private' }
JobProgressMorph >> updateLayout [

	labelMorph contents isEmpty
		ifFalse: [ self addMorphBack: labelMorph].
	self addMorphBack: bar
]
