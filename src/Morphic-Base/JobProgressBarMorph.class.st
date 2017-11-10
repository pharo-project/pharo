"
A JobProgressBarMorph is a progress bar with a title and a icon to stop update.

JobProgressBarMorph new openInWorld
"
Class {
	#name : #JobProgressBarMorph,
	#superclass : #Morph,
	#instVars : [
		'button',
		'progressBar'
	],
	#classVars : [
		'BarHeight',
		'BarWidth'
	],
	#category : #Morphic-Base-ProgressBar
}

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
	button := (self iconNamed: #stopIcon) asMorph.
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
		addMorphBack: button
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
