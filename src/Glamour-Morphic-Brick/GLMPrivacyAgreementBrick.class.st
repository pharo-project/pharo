Class {
	#name : #GLMPrivacyAgreementBrick,
	#superclass : #GLMBrick,
	#instVars : [
		'messageBrick',
		'settingsButtonBrick'
	],
	#category : #'Glamour-Morphic-Brick-Widgets-UI'
}

{ #category : #examples }
GLMPrivacyAgreementBrick class >> exampleOpen [
	<sampleInstance>

	(GLMBrick new
		hSpaceFill;
		vSpaceFill;
		addBrickBack: (
			self gtExample
				padding: 2;
				borderColor: Color lightGray;
				borderWidth: 2;
				margin: 10;
				hSpaceFill;
				vShrinkWrap;
				yourself);
		yourself)
			openInBrickWindow
			extent: 600@400
]

{ #category : #examples }
GLMPrivacyAgreementBrick class >> gtExample [
	<gtExample>
	<label: 'Simple agreement'>
	
	^ self new
]

{ #category : #utils }
GLMPrivacyAgreementBrick class >> openSettingsBrowser [
	| settingsBrowser window node nodePath |
	settingsBrowser := SettingBrowser new 
		changePackageSet: (RPackage organizer packageNamed: 'GT-EventRecorder') asOrderedCollection;
		yourself.
	window := settingsBrowser open.
	window position: Display extent - window extent // 2.
	settingsBrowser expandAll.
	node := settingsBrowser treeHolder nodeNamed: #sendDiagnosticsAndUsageData.
	nodePath := Array 
		with: (settingsBrowser treeHolder nodeNamed: node parentName)
		with: node.
	settingsBrowser selection: (MorphTreeMorphSingleSelection new selectedNodePath: nodePath).
]

{ #category : #initialization }
GLMPrivacyAgreementBrick >> initialize [
	super initialize.
	
	self useHorizontalLinearLayout.
	self hSpaceFill.
	self vShrinkWrap.
	self messageBrick: self newTitleBrick.
	self settingsButtonBrick: self newButtonBrick.
]

{ #category : #accessing }
GLMPrivacyAgreementBrick >> messageBrick [
	^ messageBrick
]

{ #category : #accessing }
GLMPrivacyAgreementBrick >> messageBrick: anObject [

	messageBrick := anObject.
	self messageBrick
		hSpaceFill;
		vAlign: #center;
		margin:10;
		text: self privacyMessage.

	self addBrickBack: self messageBrick.
]

{ #category : #'instance creation' }
GLMPrivacyAgreementBrick >> newButtonBrick [

	^ GLMButtonBrick new
]

{ #category : #'instance creation' }
GLMPrivacyAgreementBrick >> newTitleBrick [

	^ GLMRubTextAreaBrick new
]

{ #category : #actions }
GLMPrivacyAgreementBrick >> openSettingsBrowser [
	self class openSettingsBrowser
]

{ #category : #accessing }
GLMPrivacyAgreementBrick >> privacyMessage [

	^ self sendUsageData 
		ifTrue: [ 'You are sending diagnostic and usage data to help us improve Pharo. Thanks!' ]
		ifFalse: 	['No diagnostic and usage data is being sent. Would you like to send diagnostic and usage data to help us improve Pharo?']
]

{ #category : #preferences }
GLMPrivacyAgreementBrick >> sendUsageData [	
	^ self class environment at: #Privacy 
		ifPresent: [ :privacy | privacy sendDiagnosticsAndUsageData ]
		ifAbsent: [ false ].
		
]

{ #category : #accessing }
GLMPrivacyAgreementBrick >> settingsButtonBrick [
	^ settingsButtonBrick
]

{ #category : #accessing }
GLMPrivacyAgreementBrick >> settingsButtonBrick: anObject [
	settingsButtonBrick := anObject.
	
	self settingsButtonBrick
		text: self settingsButtonLabel;
		hShrinkWrap;
		vAlign: #center;
		margin:10;
		when: #onClicked do: [ self openSettingsBrowser ].
	
	self addBrickBack: self settingsButtonBrick
]

{ #category : #accessing }
GLMPrivacyAgreementBrick >> settingsButtonLabel [
	
	^ self sendUsageData 
		ifTrue: [ 'Change preferences' ]
		ifFalse: [  'Go to settings' ]
]
