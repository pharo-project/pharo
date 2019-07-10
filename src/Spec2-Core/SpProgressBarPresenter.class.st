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
	#name : #SpProgressBarPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'state'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpProgressBarPresenter class >> adapterName [

	^ #ProgressBarAdapter
]

{ #category : #api }
SpProgressBarPresenter >> fixedAt: aNumber [
	"Declare that the progress bar will be fixed at a certain value. The value should be the completed ratio between 0 and 1"
	
	self state: (SpProgressBarFixed value: aNumber)
]

{ #category : #api }
SpProgressBarPresenter >> fixedPercentage: aNumber [
	"Declare that the progress bar will be fixed at a certain value. The value should be in percent"

	self fixedAt: aNumber / 100
]

{ #category : #api }
SpProgressBarPresenter >> indeterminate [
	"If selected, the progress bar will go back and forth without any clear progression. Use it do show a progress bar with indeterminate progression."

	self state: SpProgressBarIndeterminated new
]

{ #category : #api }
SpProgressBarPresenter >> progress: aBlock [
	"Using this option, the progress bar will increase with time. The progression block should return the progression between 0 and 1."

	self progress: aBlock every: 0.2 second
]

{ #category : #api }
SpProgressBarPresenter >> progress: aBlock every: aDelay [
	"Using this option, the progress bar will increase with time. The progression block should return the progression between 0 and 1 and the delay will be used to know the delay between two refresh."

	self state: (SpProgressBarProgressing progression: aBlock every: aDelay)
]

{ #category : #accessing }
SpProgressBarPresenter >> state [
	^ state
]

{ #category : #accessing }
SpProgressBarPresenter >> state: anObject [
	state := anObject
]

{ #category : #'api-events' }
SpProgressBarPresenter >> whenValueChangedDo: aBlock [
	self state whenValueChangedDo: aBlock
]
