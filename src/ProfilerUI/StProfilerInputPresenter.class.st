"
I am a panel in the ProfilerPresenter containing user defined inputs to start profiling
"
Class {
	#name : 'StProfilerInputPresenter',
	#superclass : 'SpPresenter',
	#instVars : [
		'profilerDropList',
		'profileItButton',
		'codeInput',
		'block',
		'viewModel'
	],
	#category : 'ProfilerUI-View',
	#package : 'ProfilerUI',
	#tag : 'View'
}

{ #category : 'layout' }
StProfilerInputPresenter class >> defaultLayout [

	^ SpBoxLayout newTopToBottom
		  add: #codeInput;
		  add: (SpBoxLayout newLeftToRight
				   addLast: #profilerDropList expand: false fill: true;
				   addLast: #profileItButton expand: false fill: true;
				   yourself)
		  expand: false
		  fill: false
		  padding: 5;
		  yourself
]

{ #category : 'accessing' }
StProfilerInputPresenter >> block: aBlock [

	| sourceNode |
	codeInput
		beNotEditable;
		withoutSyntaxHighlight.

	sourceNode := aBlock sourceNode.
	sourceNode source
		ifNil: [ self code: sourceNode formattedCode ]
		ifNotNil: [ :sourceCode |
			| sourceCodeOfBlock |
			sourceCodeOfBlock := sourceCode
				                     copyFrom: sourceNode sourceInterval first + 1
				                     to: sourceNode sourceInterval last - 1.
			self code: sourceCodeOfBlock ].

	block := aBlock
]

{ #category : 'accessing' }
StProfilerInputPresenter >> code: aString [

	codeInput text: aString
]

{ #category : 'initialization' }
StProfilerInputPresenter >> initializeActions [

	profilerDropList whenSelectedItemChangedDo: [ :item |
		viewModel profilerClass: item ].

	profileItButton action: [ self profileIt ]
]

{ #category : 'initialization' }
StProfilerInputPresenter >> initializePresenters [

	codeInput := self newCode beForScripting; yourself.

	profilerDropList := self newDropList
		                    help: 'Select the desired profiler.';
		                    items: viewModel allProfilerClasses;
		                    selectItem: viewModel profilerClass;
		                    display: [ :each | each name ].

	profileItButton := self newButton
		                   icon: (self iconNamed: #glamorousGo);
		                   label: 'Profile it'.

	self focusOrder
		add: codeInput;
		add: profilerDropList;
		add: profileItButton.

	self initializeActions
]

{ #category : 'error handling' }
StProfilerInputPresenter >> notify: errorMessage at: positionIndex in: sourceCode [

	codeInput notify: errorMessage at: positionIndex in: sourceCode .
	codeInput takeKeyboardFocus
]

{ #category : 'initialization' }
StProfilerInputPresenter >> profileIt [

	block ifNotNil: [ ^ viewModel profileBlock: block ].

	codeInput text ifNotEmpty: [
		^ viewModel profileCode: codeInput text notifying: self ].

	self notify: 'Must enter code' at: 0 in: codeInput text
]

{ #category : 'accessing - model' }
StProfilerInputPresenter >> setModelBeforeInitialization: aDomainObject [

	viewModel := aDomainObject
]

{ #category : 'initialization' }
StProfilerInputPresenter >> subscribeOnProfilingAnnouncements [

	viewModel announcer
		when: ProfilingStartedAnnouncement
		do: [ self enabled: false ]
		for: self;
		when: ProfilingResultsDisplayedAnnouncement
		do: [ self enabled: true ]
		for: self
]

{ #category : 'initialization' }
StProfilerInputPresenter >> unsubscribeFromProfilingAnnouncements [
	viewModel announcer unsubscribe: self
]

{ #category : 'accessing' }
StProfilerInputPresenter >> viewModel [
	^ viewModel
]
