"
Description
--------------------

I am a presenter used to define a progress bar.

I am using a Strategy design pattern to define the kind of progress bar. For example I can be fixed at a value, indetermineted or progressing.

Examples
--------------------

	""Following example considers we are currently in a composable presenter""
	self newProgressBar indeterminate.
	
	
	self newProgressBar fixedPercentage: 30.
	self newProgressBar fixedAt: 0.3.
	
	progress := 0.
	self newProgressBar progress: [ progress := progress + 0.01 ] every: 0.5 second.
	
	progress := 0.
	self newProgressBar progress: [ progress := progress + 0.01 ].
	
	progress := 0.
	label := self newLabel.
	self newProgressBar
		progress: [ progress := progress + 0.01 ]; 
		whenValueChangedDo: [ :value | label label: (value * 100) asInteger asString , '%' ];
		yourself.
 
Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	state:		<aProgressBarState> 	Subclass of ProgressBarState to define the behavior of the progress bar (fixed, indeterminated, progressing).

"
Class {
	#name : #ProgressBarPresenter,
	#superclass : #AbstractWidgetPresenter,
	#instVars : [
		'state'
	],
	#category : #'Spec-Core-Widgets'
}

{ #category : #specs }
ProgressBarPresenter class >> adapterName [

	^ #ProgressBarAdapter
]

{ #category : #api }
ProgressBarPresenter >> fixedAt: aNumber [
	"Declare that the progress bar will be fixed at a certain value. The value should be the completed ratio between 0 and 1"
	
	self state: (ProgressBarFixed value: aNumber)
]

{ #category : #api }
ProgressBarPresenter >> fixedPercentage: aNumber [
	"Declare that the progress bar will be fixed at a certain value. The value should be in percent"

	self fixedAt: aNumber / 100
]

{ #category : #api }
ProgressBarPresenter >> indeterminate [
	"If selected, the progress bar will go back and forth without any clear progression. Use it do show a progress bar with indeterminate progression."

	self state: ProgressBarIndeterminated new
]

{ #category : #api }
ProgressBarPresenter >> progress: aBlock [
	"Using this option, the progress bar will increase with time. The progression block should return the progression between 0 and 1."

	self progress: aBlock every: 0.2 second
]

{ #category : #api }
ProgressBarPresenter >> progress: aBlock every: aDelay [
	"Using this option, the progress bar will increase with time. The progression block should return the progression between 0 and 1 and the delay will be used to know the delay between two refresh."

	self state: (ProgressBarProgressing progression: aBlock every: aDelay)
]

{ #category : #accessing }
ProgressBarPresenter >> state [
	^ state
]

{ #category : #accessing }
ProgressBarPresenter >> state: anObject [
	state := anObject
]

{ #category : #enumerating }
ProgressBarPresenter >> whenValueChangedDo: aBlock [
	self state whenValueChangedDo: aBlock
]
