"
A JobProgressBarMorph is a progress bar with a title and a icon to stop update.

		JobProgressBarMorph new openInWorld.

I am often used by the Job class in Pharo and not directly.

It is possible to disable the icon to prevent the user from stopping the task via the #isInterruptible setting.

		JobProgressBarMorph isInterruptible: false
"
Class {
	#name : 'JobProgressBarMorph',
	#superclass : 'Morph',
	#instVars : [
		'progressBar'
	],
	#classVars : [
		'IsInterruptible'
	],
	#category : 'Morphic-Base-ProgressBar',
	#package : 'Morphic-Base',
	#tag : 'ProgressBar'
}

{ #category : 'examples' }
JobProgressBarMorph class >> example [

	self new openInWorld
]

{ #category : 'settings' }
JobProgressBarMorph class >> interruptionSetting: aBuilder [
	<systemsettings>

	(aBuilder setting: #isInterruptible)
		label: 'Make progress bar interruptible';
		default: true;
		description: 'When enabled, add a button to progress bars to interrupt the action when clicked.';
		parent: #progress;
		target: self;
		order: 1
]

{ #category : 'testing' }
JobProgressBarMorph class >> isInterruptible [

	^ IsInterruptible ifNil: [ IsInterruptible := true ]
]

{ #category : 'testing' }
JobProgressBarMorph class >> isInterruptible: aBoolean [

	IsInterruptible := aBoolean
]

{ #category : 'private' }
JobProgressBarMorph >> addInterruptionButton [

	self isInterruptible ifFalse: [ ^ self ].

	self addMorphBack: (self iconNamed: #stop) asMorph
]

{ #category : 'accessing' }
JobProgressBarMorph >> decrement [

	^ progressBar decrement
]

{ #category : 'accessing' }
JobProgressBarMorph >> increment [

	^ progressBar increment
]

{ #category : 'initialization' }
JobProgressBarMorph >> initialize [

	super initialize.
	progressBar := ProgressBarMorph new.
	progressBar hResizing: #spaceFill.
	self extent: 200 @ 20.
	self
		color: Color transparent;
		layoutPolicy: TableLayout new;
		listDirection: #leftToRight;
		cellPositioning: #leftCenter;
		cellInset: 3;
		listCentering: #center;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap.

	self
		addMorphBack: progressBar;
		addInterruptionButton
]

{ #category : 'testing' }
JobProgressBarMorph >> isInterruptible [

	^ self class isInterruptible
]

{ #category : 'accessing' }
JobProgressBarMorph >> progress [

	^ progressBar value / 100
]

{ #category : 'accessing' }
JobProgressBarMorph >> progress: aNormalizedNumber [

	^ progressBar value: aNormalizedNumber * 100
]

{ #category : 'evaluating' }
JobProgressBarMorph >> value [

	^ progressBar value
]

{ #category : 'accessing' }
JobProgressBarMorph >> value: aNumber [

	^ progressBar value: aNumber
]
