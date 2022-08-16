"
I am a panel in the ProfilerPresenter containing user defined inputs to start profiling
"
Class {
	#name : #ProfilerInputPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'profilerDropList',
		'profileItButton',
		'codeInput',
		'viewModel'
	],
	#category : #'ProfilerUI-View'
}

{ #category : #initialization }
ProfilerInputPresenter >> initializeActions [

	profilerDropList whenSelectedItemChangedDo: [ :item | 
		viewModel profilerClass: item ].


	profileItButton action: [ 
		codeInput text 
			ifEmpty: [ self notify: 'Must enter code' at: 0 in: codeInput text ]
			ifNotEmpty: [ viewModel profileCode: codeInput text notifying: self] ]
]

{ #category : #initialization }
ProfilerInputPresenter >> initializePresenters [

	codeInput := self newCode beForScripting.

	profilerDropList := self newDropList
		                    help: 'Select the desired profiler.';
		                    items: viewModel allProfilerClasses;
		                    selectItem: viewModel profilerClass;
		                    display: [ :each | each name ].

	profileItButton := self newButton
		                   icon: (self iconNamed: #glamorousGo);
		                   label: 'Profile it'.

	self layout: (SpBoxLayout newTopToBottom
			 add: (SpBoxLayout newLeftToRight
					  add: profileItButton height: self class toolbarHeight;
					  add: profilerDropList height: self class buttonHeight;
					  yourself)
			 expand: false
			 fill: false
			 padding: 5;
			
			 add: codeInput height: self class toolbarHeight*10;
			 yourself).

	self focusOrder
		add: codeInput;
		add: profileItButton.

	self initializeActions
]

{ #category : #'error handling' }
ProfilerInputPresenter >> notify: errorMessage at: positionIndex in: sourceCode [ 

	codeInput notify: errorMessage at: positionIndex in: sourceCode .
	codeInput takeKeyboardFocus

]

{ #category : #'accessing - model' }
ProfilerInputPresenter >> setModelBeforeInitialization: aDomainObject [

	viewModel := aDomainObject
]

{ #category : #initialization }
ProfilerInputPresenter >> subscribeOnProfilingAnnouncements [
	viewModel announcer 
		when: ProfilingStartedAnnouncement 
		do: [ self enabled: false ];
		
		when: ProfilingFinishedAnnouncement 
		do: [ self enabled: true ].
	
]

{ #category : #initialization }
ProfilerInputPresenter >> unsubscribeFromProfilingAnnouncements [
	viewModel announcer unsubscribe: self
]

{ #category : #accessing }
ProfilerInputPresenter >> viewModel [
	^ viewModel
]
