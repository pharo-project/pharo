"
I am a graphical user interface for profiling code using both AndreasProfiler and TimeProfiler
"
Class {
	#name : 'ProfilerPresenter',
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
ProfilerPresenter class >> defaultLayout [
	^ SpPanedLayout newTopToBottom
		  positionOfSlider: 20 percent;
		  add: #inputPresenter;
		  add: #resultsPresenter;
		  yourself
]

{ #category : 'examples' }
ProfilerPresenter class >> menuCommandOn: aBuilder [
	<worldMenu>
	(aBuilder item: #'Profiler')
		parent: #Profiling;
		order: 1;
		icon: (self iconNamed: #smallProfile);
		help: 'Profile the execution of a piece of code using different profilers, and navigate into the result.';
		action: [self new open]
]

{ #category : 'examples' }
ProfilerPresenter class >> open [

	<example>
	^ self new open
]

{ #category : 'spying' }
ProfilerPresenter class >> profileCode: aString [

	^ self new
		  open;
		  profileCode: aString asString;
		  yourself
]

{ #category : 'constants' }
ProfilerPresenter >> initExtent [
	^ 800 @ 800
]

{ #category : 'initialization' }
ProfilerPresenter >> initializePresenters [

	inputPresenter := self newInputPresenter.
	resultsPresenter := self newResultsPresenter
		                    enabled: false;
		                    yourself
]

{ #category : 'initialization' }
ProfilerPresenter >> initializeWindow: aWindowPresenter [
	aWindowPresenter
		title: self presenterTitle;
		initialExtent: self initExtent;
		whenOpenedDo: [ self subscribe ];
		whenClosedDo: [ self unsubscribe ]
]

{ #category : 'private - factory' }
ProfilerPresenter >> newInputPresenter [

	^ ProfilerInputPresenter on: self viewModel
]

{ #category : 'private - factory' }
ProfilerPresenter >> newResultsPresenter [

	^ ProfilerResultsPresenter on: self viewModel
]

{ #category : 'constants' }
ProfilerPresenter >> presenterTitle [
	^ 'Profiler'
]

{ #category : 'actions' }
ProfilerPresenter >> profileCode: aString [

	inputPresenter code: aString.
	inputPresenter profileIt
]

{ #category : 'subscription' }
ProfilerPresenter >> subscribe [

	resultsPresenter subscribeOnProfilingAnnouncements.
	inputPresenter subscribeOnProfilingAnnouncements
]

{ #category : 'subscription' }
ProfilerPresenter >> unsubscribe [

	resultsPresenter unsubscribeFromProfilingAnnouncements.
	inputPresenter unsubscribeFromProfilingAnnouncements
]

{ #category : 'accessing' }
ProfilerPresenter >> viewModel [

	^ viewModel ifNil: [ viewModel := ProfilerModel new ]
]
