Extension { #name : #GTSpotter }

{ #category : #'*GT-Spotter-UI' }
GTSpotter class >> exampleInWindow [
	<sampleInstance>
	
	^ GTSpotterMorph new
		extent: 400@500;
		doLayout;
		spotterModel: self new;
		openInWindow
]

{ #category : #'*GT-Spotter-UI' }
GTSpotter class >> exampleMC [
	"self exampleMC"
	<sampleInstance>
	^ GTSpotterMorph new
		extent: 400@500;
		doLayout;
		spotterModel: (self on: MCWorkingCopy allManagers);
		openCenteredInWorld
]

{ #category : #'*GT-Spotter-UI' }
GTSpotter class >> exampleWithPreview [
	<sampleInstance>
	^ GTSpotterMorph new
		extent: (World width / 2.5 @ (World height / 1.5)) asIntegerPoint;
		spotterModel: self new;
		showPreview;
		openCenteredInWorld
]

{ #category : #'*GT-Spotter-UI' }
GTSpotter class >> exampleWorldMenu [
	<sampleInstance>
	^ GTSpotterMorph new
		extent: 400@500;
		doLayout;
		spotterModel: (self on: World worldMenu);
		openCenteredInWorld
]

{ #category : #'*GT-Spotter-UI' }
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

{ #category : #'*GT-Spotter-UI' }
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

{ #category : #'*GT-Spotter-UI' }
GTSpotter >> onPagePopped [
	
	self steps size > 0 ifTrue: [
		self currentStep: self steps last ]
]

{ #category : #'*GT-Spotter-UI' }
GTSpotter >> pagerModel [

	^ pagerModel ifNil: [
		pagerModel := self defaultPagerModel defaultNumberOfVisiblePages: 1.
		pagerModel announcer when: GLMPagePoped send: #onPagePopped to: self.
		pagerModel ]
]

{ #category : #'*GT-Spotter-UI' }
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

{ #category : #'*GT-Spotter-UI' }
GTSpotter >> popUntil: aStep [

	[ self steps last = aStep ]
		whileFalse: [ self popStep ]
]

{ #category : #'*GT-Spotter-UI' }
GTSpotter >> requestNextCategoryStepWith: aProcessorLink for: anEventType [
	|collection|
	
	collection := aProcessorLink processor allFilteredCandidates
		collect: [ :each | each asSpotterCandidateLink renderingProcessor: aProcessorLink ].
		
	self requestNextStepWith: collection for: anEventType
]
