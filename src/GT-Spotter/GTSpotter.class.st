"
I am  the main model class.

I handle the list of current search steps and provide the main functionality for spawning new search steps or removing old ones.
I maintain search steps as a stack.
 
I  trigger a search when the user enters a query (#setText:from:) and maintain the search text.

To avoid crashes due to exceptions in search processors I can install an exception handler that prints exceptions to the transcript instead of opening a debugger (#beDeployment). Afternatively I can install an exception handler that  propagates exceptions (#beDeployment)
"
Class {
	#name : #GTSpotter,
	#superclass : #Object,
	#instVars : [
		'foundationOrigin',
		'steps',
		'announcer',
		'currentStep',
		'pagerModel',
		'text',
		'isPreviewVisible',
		'isShiftPressed',
		'exceptionHandler',
		'history',
		'processorsFilter'
	],
	#classInstVars : [
		'history',
		'isPreviewVisible',
		'wasOpened'
	],
	#category : #'GT-Spotter-Model'
}

{ #category : #cleanup }
GTSpotter class >> cleanUp [
	self resetHistory.
	wasOpened := false
]

{ #category : #accessing }
GTSpotter class >> dontShowPreview [

	isPreviewVisible := false
]

{ #category : #example }
GTSpotter class >> exampleInWindow [
	<sampleInstance>
	
	^ GTSpotterMorph new
		extent: 400@500;
		doLayout;
		spotterModel: self new;
		openInWindow
]

{ #category : #example }
GTSpotter class >> exampleMC [
	"self exampleMC"
	<sampleInstance>
	^ GTSpotterMorph new
		extent: 400@500;
		doLayout;
		spotterModel: (self on: MCWorkingCopy allManagers);
		openCenteredInWorld
]

{ #category : #example }
GTSpotter class >> exampleSpotterWithoutMorph [
	<sampleInstance>
	<gtExample>
	<label: 'A GTSpotter without UI'>
	
	^ self new
]

{ #category : #example }
GTSpotter class >> exampleWithPreview [
	<sampleInstance>
	^ GTSpotterMorph new
		extent: (World width / 2.5 @ (World height / 1.5)) asIntegerPoint;
		spotterModel: self new;
		showPreview;
		openCenteredInWorld
]

{ #category : #example }
GTSpotter class >> exampleWorldMenu [
	<sampleInstance>
	^ GTSpotterMorph new
		extent: 400@500;
		doLayout;
		spotterModel: (self on: World worldMenu);
		openCenteredInWorld
]

{ #category : #accessing }
GTSpotter class >> extensionsPragma [
	^ #spotterOrder:
]

{ #category : #inspecting }
GTSpotter class >> gtInspectorExtensionsIn: composite [
	<gtInspectorPresentationOrder: 40>
	composite list
		title: 'Extensions';
		display: [ self spotterExtendingMethods ];
		showOnly: 50;
		format: [ :each | each gtDisplayString ];
		filterOn: [ :text :each | each gtDisplayString includesSubstring: text caseSensitive: false ]
]

{ #category : #accessing }
GTSpotter class >> history [
	^ history ifNil: [ history := WeakOrderedCollection new ]
]

{ #category : #testing }
GTSpotter class >> isPreviewVisible [

	^ isPreviewVisible ifNil: [ isPreviewVisible := false ]
]

{ #category : #accessing }
GTSpotter class >> markOpened [

	wasOpened := true
]

{ #category : #'world menu' }
GTSpotter class >> menuCommandOn: aBuilder [
	<worldMenu>
	(aBuilder item: #Spotter)
		action: [ GTSpotterGlobalShortcut openGlobalSpotter ];
		keyText: 'Shift + Enter';
		order: 50;
		parent: #Tools;
		help: 'Search tool to explore Pharo system effectively.';
		iconName: #smallFindIcon
]

{ #category : #'instance creation' }
GTSpotter class >> on: anObject [
	
	^ self new foundationOrigin: anObject
]

{ #category : #accessing }
GTSpotter class >> resetHistory [
	"
	self resetHistory
	"
	^ history := nil
]

{ #category : #accessing }
GTSpotter class >> showPreview [

	isPreviewVisible := true
]

{ #category : #inspecting }
GTSpotter class >> spotterExtendingMethods [
	^(SystemNavigation default allBehaviors
		flatCollect: [ :each | 
			(Pragma allNamed: self extensionsPragma in: each) 
				collect: [ :p | p method asRingDefinition ] ])
		sorted
]

{ #category : #testing }
GTSpotter class >> wasOpened [

	^ wasOpened ifNil: [ false ]
]

{ #category : #'actions api' }
GTSpotter >> addStep: aStepModel [

	self steps add: aStepModel.
	self notifyStepAdded: aStepModel
]

{ #category : #'actions api' }
GTSpotter >> addStep: aSpotterStep during: aBlock [
	"Adds aSpotterStep without announcements,
	evaluates aBlock and removes previously added
	step after evaluation.
	May be used to determine if it is possible to dive in
	a candidate by adding a fake step to simulate dive in.
	aSpotterStep must not be nil
	aBlock must not be nil"
	self assert: aSpotterStep isNotNil.
	self assert: aBlock isNotNil.
	
	self steps add: aSpotterStep.
	aBlock ensure: [ 
		self steps remove: aSpotterStep ]
]

{ #category : #'actions api' }
GTSpotter >> after: afterBlock do: doBlock ifTrue: conditionBlock [
	|value|
	value := conditionBlock brickValue: self.
	afterBlock brickValue: self.
	value ifTrue: [ doBlock brickValue: value ]
]

{ #category : #'actions api' }
GTSpotter >> after: afterBlock do: doBlock with: withBlock [
	|value|
	value := withBlock brickValue: self.
	afterBlock brickValue: self.
	doBlock brickValue: value
]

{ #category : #notifying }
GTSpotter >> announce: anAnnouncement [
	
	self announcer announce: anAnnouncement
]

{ #category : #accessing }
GTSpotter >> announcer [

	^ announcer ifNil: [ announcer := Announcer new ]
]

{ #category : #'accessing-convenience' }
GTSpotter >> beDebugging [
	self exceptionHandler: GTSpotterDebuggingExceptionHandler new
]

{ #category : #'accessing-convenience' }
GTSpotter >> beDeployment [
	self exceptionHandler: GTSpotterDeploymentExceptionHandler new
]

{ #category : #accessing }
GTSpotter >> currentStep [
	^ currentStep
		ifNil: [ self hasSteps
				ifTrue: [ currentStep := self steps last ]
				ifFalse: [ self newStep origin: self foundationOrigin ] ]
]

{ #category : #accessing }
GTSpotter >> currentStep: aStepModel [
	
	currentStep := aStepModel.
	self notifyCurrentStepChanged
]

{ #category : #'accessing-defaults' }
GTSpotter >> defaultExceptionHandler [
	^ GTSpotterDeploymentExceptionHandler new
]

{ #category : #'accessing-defaults' }
GTSpotter >> defaultPagerModel [
	^ GLMPagerModel new
]

{ #category : #'accessing-defaults' }
GTSpotter >> defaultStep [
	^ GTSpotterStep new
]

{ #category : #'actions api' }
GTSpotter >> doIt [
	| result |
	Smalltalk compiler
		silentlyDo: [ result := Smalltalk compiler evaluate: text string trimBoth ]
		exceptionDo: [:ex | ^ex ]. 
	"self exitDueTo: #doIt"
]

{ #category : #accessing }
GTSpotter >> exceptionHandler [
	^ exceptionHandler ifNil: [ exceptionHandler := self defaultExceptionHandler ]
]

{ #category : #accessing }
GTSpotter >> exceptionHandler: anObject [
	exceptionHandler := anObject
]

{ #category : #'actions api' }
GTSpotter >> exit [

	self exitDueTo: #unknown
]

{ #category : #'actions api' }
GTSpotter >> exitDueTo: anEventType [

	self notifySpotterExitFor: anEventType
]

{ #category : #filtering }
GTSpotter >> filterByMethods: aCollection [
	| filter |
	
	filter := GTSpotterMethodListProcessorFilter new.
	aCollection  do: [ :aMethod |
		filter allowProcessorsFrom: aMethod ].
	self processorsFilter: filter
		
]

{ #category : #accessing }
GTSpotter >> foundationOrigin [
	
	^ foundationOrigin ifNil: [ foundationOrigin := self ]
]

{ #category : #accessing }
GTSpotter >> foundationOrigin: anObject [
	
	foundationOrigin := anObject
]

{ #category : #'model access' }
GTSpotter >> getText [

	^ self text
]

{ #category : #printing }
GTSpotter >> gtDisplayOn: stream [

	stream nextPutAll: 'Spotter'
]

{ #category : #printing }
GTSpotter >> gtDisplaySpotterMorphOn: aComposite [
	<gtInspectorPresentationOrder: 50>
	
	aComposite morph 
		title: 'Live';
		display: [ 
			GLMBrick new 
				hSpaceFill;
				vSpaceFill;
				addMorphBack: 
					(GTSpotterMorph new
						spotterModel: self);
				yourself ]
]

{ #category : #inspecting }
GTSpotter >> gtInspectorStepsIn: composite [
	<gtInspectorPresentationOrder: 40>
	composite list
		title: 'Steps';
		display: [ self steps ];
		when: [ self steps notEmpty ]
]

{ #category : #'event handling' }
GTSpotter >> handleExceptionsDuring: aBlock [
	self exceptionHandler handleExceptionsDuring: aBlock
]

{ #category : #testing }
GTSpotter >> hasMultipleSteps [

	^ self steps size > 1
]

{ #category : #testing }
GTSpotter >> hasSelected [
	^ self hasSteps and: [ self currentStep hasSelected ]
]

{ #category : #testing }
GTSpotter >> hasSteps [
	^ 	self steps isNotEmpty
]

{ #category : #'actions api' }
GTSpotter >> hidePreview [

	self isPreviewVisible ifFalse: [ ^ self ].
	isPreviewVisible := false.
	self class dontShowPreview.
	self notifyHidePreview
	
]

{ #category : #'actions api' }
GTSpotter >> historize: target [

	(target = self or: [ target isNil ])
		ifFalse: [ 
			self history removeAllSuchThat: [ :each | each value = target value ] .
			self history addFirst: target ]
]

{ #category : #accessing }
GTSpotter >> history [
	^ history ifNil: [ self class history ]
]

{ #category : #accessing }
GTSpotter >> history: aWeakCollection [
	history := aWeakCollection
]

{ #category : #initialization }
GTSpotter >> initialize [
	super initialize.
	
	processorsFilter := GTSpotterSettingsProcessorFilter new
]

{ #category : #'actions api' }
GTSpotter >> inspectIt [
	| result |
	Smalltalk compiler
		silentlyDo: [ result := Smalltalk compiler evaluate: text string trimBoth ]
		exceptionDo: [ :ex | ^ex  ].
	result inspect.
	self exitDueTo: #inspectIt 
]

{ #category : #testing }
GTSpotter >> isEmpty [
	self deprecated: 'Use hasSteps instead'.
	^ self hasSteps not
]

{ #category : #testing }
GTSpotter >> isPreviewVisible [

	^ isPreviewVisible ifNil: [ isPreviewVisible := false ]
]

{ #category : #accessing }
GTSpotter >> isShiftPressed [

	^ isShiftPressed ifNil: [ isShiftPressed := false ]
]

{ #category : #protocol }
GTSpotter >> menu [

	^ nil
]

{ #category : #'instance creation' }
GTSpotter >> newStep [

	^ self defaultStep
		spotter: self
]

{ #category : #notifying }
GTSpotter >> notifyActOn: anObject for: anEventType [

	self announce: (GTSpotterActOn new 
		element: anObject;
		eventType: anEventType)
]

{ #category : #notifying }
GTSpotter >> notifyCandidateSelected: anObject for: anEventType [

	self announce: (GTSpotterSelectedCandidateChanged new 
		select: anObject;
		eventType: anEventType)
]

{ #category : #notifying }
GTSpotter >> notifyCategorySelected: aProcessor [

	self announce: (GTSpotterCategorySelected new category: aProcessor)
]

{ #category : #notifying }
GTSpotter >> notifyContextChangedFrom: aPreviousContext to: aCurrentContext [
	self announce: (GTSpotterContextChanged new
		previous: aPreviousContext;
		current: aCurrentContext;
		yourself)
]

{ #category : #notifying }
GTSpotter >> notifyCurrentStepChanged [

	self announce: GTSpotterCurrentStepChanged new
]

{ #category : #notifying }
GTSpotter >> notifyDiveIn: anObject for: anEventType [

	self announce: (GTSpotterDiveInElement new 
		element: anObject;
		eventType: anEventType )
]

{ #category : #notifying }
GTSpotter >> notifyDiveInCategory: aProcessor for: anEventType [

	self announce: (GTSpotterDiveInCategory new 
		category: aProcessor;
		eventType: anEventType)
]

{ #category : #notifying }
GTSpotter >> notifyDiveOut: anObject [

	self announce: (GTSpotterDiveOut new element: anObject)
]

{ #category : #notifying }
GTSpotter >> notifyHideHints [

	self announce: GTSpotterHideHints new
]

{ #category : #notifying }
GTSpotter >> notifyHidePreview [

	self announce: GTSpotterHidePreview new
]

{ #category : #notifying }
GTSpotter >> notifyRevealHints [

	self announce: GTSpotterRevealHints new
]

{ #category : #notifying }
GTSpotter >> notifySearchFinishedFor: aString processors: aCollectionOfProcessors [

	self announce: (GTSpotterSearchFinished new 
								query: aString;
								processors: aCollectionOfProcessors;
								yourself)
]

{ #category : #notifying }
GTSpotter >> notifyShowPreview [

	self announce: GTSpotterShowPreview new

	
]

{ #category : #notifying }
GTSpotter >> notifySpotterExitFor: anEventType [

	self announce: (GTSpotterExitAnnouncement new
		eventType: anEventType)
]

{ #category : #notifying }
GTSpotter >> notifyStepAdded: aStep [

	self announce: (GTSpotterStepAdded new stepModel: aStep)
]

{ #category : #notifying }
GTSpotter >> notifyStepPopped [

	self announce: GTSpotterStepPopped new
]

{ #category : #'event handling' }
GTSpotter >> onPagePopped [
	
	self steps size > 0 ifTrue: [
		self currentStep: self steps last ]
]

{ #category : #accessing }
GTSpotter >> pagerModel [

	^ pagerModel ifNil: [
		pagerModel := self defaultPagerModel defaultNumberOfVisiblePages: 1.
		pagerModel announcer when: GLMPagePoped send: #onPagePopped to: self.
		pagerModel ]
]

{ #category : #'actions api' }
GTSpotter >> popStep [

	"there always should be minimum one step left"
	(self steps size > 1)
		ifTrue: [
			self
				after: [ self steps removeLast ]
				do: [ :o | self notifyDiveOut: o ]
				with: self currentStep origin.
			self notifyStepPopped.
			"perform pane popping only after we scrolled to the penultimate pane, so user doesn't see blinking"
			self pagerModel
				scrollToPane: self steps size
				callback: [ self pagerModel popPane: false ] ]
]

{ #category : #'actions api' }
GTSpotter >> popUntil: aStep [

	[ self steps last = aStep ]
		whileFalse: [ self popStep ]
]

{ #category : #accessing }
GTSpotter >> processorsFilter [

	^ processorsFilter
]

{ #category : #accessing }
GTSpotter >> processorsFilter: aFilter [
   "Set the filter for determining whether or not processors get added to a step."

	processorsFilter := aFilter
]

{ #category : #'actions api' }
GTSpotter >> pushStep: stepModel [
	
	self addStep: stepModel.
	"updating current step"
	self currentStep: stepModel
]

{ #category : #'event handling' }
GTSpotter >> requestNextCategoryStepWith: aProcessorLink for: anEventType [
	|collection|
	
	collection := aProcessorLink processor allFilteredCandidates
		collect: [ :each | each asSpotterCandidateLink renderingProcessor: aProcessorLink ].
		
	self requestNextStepWith: collection for: anEventType
]

{ #category : #'event handling' }
GTSpotter >> requestNextStepWith: origin for: anEventType [

	text := ''.
	self pushStep: (self newStep origin: origin; event: anEventType; yourself).
	self steps last process: nil
]

{ #category : #'model access' }
GTSpotter >> setText: aText from: aView [
	"I am the main entry point for triggering a new search in the current step."
	
	self steps isEmpty
		"by default there are no steps, so we need to create one based on foundation origin"
		ifTrue: [ self addStep: ( self newStep origin: self foundationOrigin ) ].

	(aText asString isEmpty and: [ self text isEmpty not ])
		ifTrue: [ self currentStep process: nil ]
		ifFalse: [ self currentStep process: aText asString ].
	
	text := aText asString asText.
	^ true
]

{ #category : #'actions api' }
GTSpotter >> shiftPressed: aBoolean [

	self isShiftPressed = aBoolean ifTrue: [ ^ self ].
	
	isShiftPressed := aBoolean.
	
	SmalltalkImage current vm canHandleShiftPressed
		ifTrue: [ ^ isShiftPressed := false ].

	self isShiftPressed
		ifTrue: [ self notifyRevealHints ]
		ifFalse: [ self notifyHideHints ]

	
]

{ #category : #'actions api' }
GTSpotter >> showPreview [

	self isPreviewVisible ifTrue: [ ^ self ].
	isPreviewVisible := true.
	self class showPreview.
	self notifyShowPreview
	
]

{ #category : #accessing }
GTSpotter >> steps [

	^ steps ifNil: [ steps := OrderedCollection new ]
]

{ #category : #'actions api' }
GTSpotter >> terminateProcessors [

	self steps do: [ :each | each terminateProcessors ]
]

{ #category : #accessing }
GTSpotter >> text [

	^ text ifNil: [ text := '' asText ]
]

{ #category : #'actions api' }
GTSpotter >> togglePreview [

	self isPreviewVisible
		ifTrue: [ self hidePreview ]
		ifFalse: [ self showPreview ]
]
