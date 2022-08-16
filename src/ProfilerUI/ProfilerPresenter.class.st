"
I am a graphical user interface for profiling code using both AndreasProfiler and TimeProfiler
"
Class {
	#name : #ProfilerPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'inputPresenter',
		'resultsPresenter',
		'viewModel'
	],
	#category : #'ProfilerUI-View'
}

{ #category : #examples }
ProfilerPresenter class >> menuCommandOn: aBuilder [ 
	<worldMenu>
	(aBuilder item: #'Andreas & Time Profiler')
		parent: #Profiling;  
		order: 1;
		icon: (self iconNamed: #smallProfile);
		help: 'Profile the execution of a piece of code and navigate into the result.';
		action: [self new open]
]

{ #category : #examples }
ProfilerPresenter class >> open [

	<example>
	^ self new open
]

{ #category : #constants }
ProfilerPresenter >> initExtent [
	^ 800 @ 800
]

{ #category : #initialization }
ProfilerPresenter >> initializePresenters [

	self layout: (SpBoxLayout newTopToBottom
			 add: (inputPresenter := self newInputPresenter) height: 100;
			 add: (resultsPresenter := self newResultsPresenter
					                      enabled: false;
					                      yourself)
			 expand: true;
			 yourself)
]

{ #category : #initialization }
ProfilerPresenter >> initializeWindow: aWindowPresenter [
	aWindowPresenter
		title: self presenterTitle;
		initialExtent: self initExtent;
		whenOpenedDo: [ self subscribe ];
		whenClosedDo: [ self unsubscribe ]
]

{ #category : #'private - factory' }
ProfilerPresenter >> newInputPresenter [

	^ ProfilerInputPresenter on: self viewModel
]

{ #category : #'private - factory' }
ProfilerPresenter >> newResultsPresenter [

	^ ProfilerResultsPresenter on: self viewModel
]

{ #category : #constants }
ProfilerPresenter >> presenterTitle [
	^ 'Profiler'
]

{ #category : #subscription }
ProfilerPresenter >> subscribe [
	
	resultsPresenter subscribeOnProfilingAnnouncements.
	inputPresenter subscribeOnProfilingAnnouncements
	
]

{ #category : #subscription }
ProfilerPresenter >> unsubscribe [
	
	resultsPresenter unsubscribeFromProfilingAnnouncements.
	inputPresenter unsubscribeFromProfilingAnnouncements.
	
]

{ #category : #accessing }
ProfilerPresenter >> viewModel [

	^ viewModel ifNil: [ viewModel := ProfilerModel new ]
]
