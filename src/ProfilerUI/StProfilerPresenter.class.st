"
I am a graphical user interface for profiling code using both AndreasProfiler and TimeProfiler
"
Class {
	#name : 'StProfilerPresenter',
	#superclass : 'SpPresenter',
	#instVars : [
		'inputPresenter',
		'resultsPresenter',
		'viewModel'
	],
	#category : 'ProfilerUI-View',
	#package : 'ProfilerUI',
	#tag : 'View'
}

{ #category : 'layout' }
StProfilerPresenter class >> defaultLayout [
	^ SpPanedLayout newTopToBottom
		  positionOfSlider: 20 percent;
		  add: #inputPresenter;
		  add: #resultsPresenter;
		  yourself
]

{ #category : 'examples' }
StProfilerPresenter class >> menuCommandOn: aBuilder [
	<worldMenu>
	(aBuilder item: #'Profiler')
		parent: #Profiling;
		order: 1;
		icon: (self iconNamed: #smallProfile);
		help: 'Profile the execution of a piece of code using different profilers, and navigate into the result.';
		action: [self new open]
]

{ #category : 'examples' }
StProfilerPresenter class >> open [

	<example>
	^ self new open
]

{ #category : 'spying' }
StProfilerPresenter class >> profileBlock: aBlock [

	^ self new
		  open;
		  profileBlock: aBlock;
		  yourself
]

{ #category : 'spying' }
StProfilerPresenter class >> profileCode: aString [

	^ self new
		  open;
		  profileCode: aString asString;
		  yourself
]

{ #category : 'constants' }
StProfilerPresenter >> initExtent [
	^ 800 @ 800
]

{ #category : 'initialization' }
StProfilerPresenter >> initializePresenters [

	inputPresenter := self newInputPresenter.
	resultsPresenter := self newResultsPresenter
		                    enabled: false;
		                    yourself
]

{ #category : 'initialization' }
StProfilerPresenter >> initializeWindow: aWindowPresenter [
	aWindowPresenter
		title: self presenterTitle;
		initialExtent: self initExtent;
		whenOpenedDo: [ self subscribe ];
		whenClosedDo: [ self unsubscribe ]
]

{ #category : 'private - factory' }
StProfilerPresenter >> newInputPresenter [

	^ StProfilerInputPresenter on: self viewModel
]

{ #category : 'private - factory' }
StProfilerPresenter >> newResultsPresenter [

	^ StProfilerResultsPresenter on: self viewModel
]

{ #category : 'constants' }
StProfilerPresenter >> presenterTitle [
	^ 'Profiler'
]

{ #category : 'actions' }
StProfilerPresenter >> profileBlock: aBlock [

	inputPresenter
		block: aBlock;
		profileIt
]

{ #category : 'actions' }
StProfilerPresenter >> profileBlock: aBlock displayCode: aString [

	inputPresenter
		block: aBlock;
		code: aString;
		profileIt
]

{ #category : 'actions' }
StProfilerPresenter >> profileCode: aString [

	inputPresenter
		code: aString;
		profileIt
]

{ #category : 'subscription' }
StProfilerPresenter >> subscribe [

	resultsPresenter subscribeOnProfilingAnnouncements.
	inputPresenter subscribeOnProfilingAnnouncements
]

{ #category : 'subscription' }
StProfilerPresenter >> unsubscribe [

	resultsPresenter unsubscribeFromProfilingAnnouncements.
	inputPresenter unsubscribeFromProfilingAnnouncements
]

{ #category : 'accessing' }
StProfilerPresenter >> viewModel [

	^ viewModel ifNil: [ viewModel := ProfilerModel new ]
]
