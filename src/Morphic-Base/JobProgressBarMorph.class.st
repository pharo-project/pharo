"
A JobProgressBarMorph is a progress bar with a title and a icon to stop update.

		JobProgressBarMorph new openInWorld.

I am often used by the Job class in Pharo and not directly.

It is possible to disable the icon to prevent the user from stopping the task via the #isInterruptable setting.

		JobProgressBarMorph isInterruptable: false
"
Class {
	#name : #JobProgressBarMorph,
	#superclass : #Morph,
	#instVars : [
		'progressBar'
	],
	#classVars : [
		'BarHeight',
		'BarWidth',
		'IsInterruptable'
	],
	#category : #'Morphic-Base-ProgressBar'
}

{ #category : #settings }
JobProgressBarMorph class >> interruptionSetting: aBuilder [ 
	<systemsettings>
	(aBuilder setting: #isInterruptable) 
		label: 'Make progress bar interuptable';
		default: true;
		description: 'When enabled, add a button to progress bars to interupt the action when clicked.';
		parent: #progress; 
		target: self;
		order: 1
]

{ #category : #accessing }
JobProgressBarMorph class >> isInterruptable [
	^ IsInterruptable ifNil: [ IsInterruptable := true ]
]

{ #category : #accessing }
JobProgressBarMorph class >> isInterruptable: anObject [
	IsInterruptable := anObject
]

{ #category : #initialization }
JobProgressBarMorph >> addInterruptionButton [
	self isInterruptable ifFalse: [ ^ self ].

	self addMorphBack: (self iconNamed: #stopIcon) asMorph.
]

{ #category : #accessing }
JobProgressBarMorph >> decrement [
	^ progressBar decrement
]

{ #category : #accessing }
JobProgressBarMorph >> increment [
	^ progressBar increment
]

{ #category : #initialization }
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
	self addMorphBack: progressBar.
	self addInterruptionButton
]

{ #category : #testing }
JobProgressBarMorph >> isInterruptable [ 
	^ self class isInterruptable
]

{ #category : #accessing }
JobProgressBarMorph >> progress [
	^ progressBar value / 100
]

{ #category : #accessing }
JobProgressBarMorph >> progress: aNormalizedNumber [
	^ progressBar value: aNormalizedNumber * 100
]

{ #category : #evaluating }
JobProgressBarMorph >> value [
	^ progressBar value
]

{ #category : #accessing }
JobProgressBarMorph >> value: aNumber [
	^ progressBar value: aNumber
]
