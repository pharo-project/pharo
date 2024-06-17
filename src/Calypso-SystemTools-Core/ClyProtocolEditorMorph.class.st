"
I am status bar tool for method editors to select a protocol or extending package for the editing method.

While system only support protocol when method is not extension I do not show both elements.
So I provide checkbox for extension status.
When it is active I should extending package in the label.
When it is not active I show the protocol in the label.

When user toggle checkbox I request either package or protocol depending on requested mode of method.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	editButton:		<Morph>
	extensionCheckbox:		<Morph>
	label:		<StringMorph>

"
Class {
	#name : 'ClyProtocolEditorMorph',
	#superclass : 'ClyStatusBarItemMorph',
	#instVars : [
		'extensionCheckbox',
		'editButton',
		'label',
		'resetButton'
	],
	#category : 'Calypso-SystemTools-Core-Editors-Methods',
	#package : 'Calypso-SystemTools-Core',
	#tag : 'Editors-Methods'
}

{ #category : 'building' }
ClyProtocolEditorMorph >> build [
	self buildResetButton.
	self addMorphBack: resetButton.

	self buildEditButton.
	self addMorphBack: editButton.

	self buildLabel.
	self addMorphBack: label.

	self buildExtensionCheckBoxButton.
	self addMorphBack: extensionCheckbox.

	self update
]

{ #category : 'building' }
ClyProtocolEditorMorph >> buildEditButton [
	editButton := IconicButtonMorph new
		target: self;
		actionSelector: #openEditor;
		arguments: #();
		labelFormSet: (self iconFormSetNamed: #edit);
		color: Color transparent;
		borderWidth: 0
]

{ #category : 'building' }
ClyProtocolEditorMorph >> buildExtensionCheckBoxButton [
	extensionCheckbox := self theme
		newCheckboxIn: self
		for: self
		getSelected: #isExtensionActive
		setSelected: #toggleExtension
		getEnabled: nil
		label: 'extension'
		help: 'toggle check box to choose/reset package of method'.
	extensionCheckbox
		changeTableLayout;
		vResizing: #shrinkWrap;
		hResizing: #shrinkWrap
]

{ #category : 'building' }
ClyProtocolEditorMorph >> buildLabel [

	label := self theme newLabelIn: self label: self printProtocolOrPackage.
	label on: #click send: #openEditor to: self
]

{ #category : 'building' }
ClyProtocolEditorMorph >> buildResetButton [

	resetButton := IconicButtonMorph new
		target: self;
		actionSelector: #resetProtocolAndPackage;
		arguments: #();
		labelFormSet: (self iconFormSetNamed: #glamorousClose);
		color: Color transparent;
		borderWidth: 0;
		hResizing: #shinkWrap;
		layoutInset: 4@0
]

{ #category : 'initialization' }
ClyProtocolEditorMorph >> initialize [
	super initialize.
	self cellInset: 2@2
]

{ #category : 'testing' }
ClyProtocolEditorMorph >> isExtensionActive [

	^ownerTool extendingPackage isNotNil
]

{ #category : 'operations' }
ClyProtocolEditorMorph >> openEditor [

	self requestChangeBy: [
		self isExtensionActive
			ifTrue: [ self requestPackage ]
			ifFalse: [ self requestProtocol ] ]
]

{ #category : 'accessing' }
ClyProtocolEditorMorph >> ownerTool: anObject [

	super ownerTool: anObject
]

{ #category : 'printing' }
ClyProtocolEditorMorph >> printProtocol [

	^ ownerTool methodProtocol ifNil: [ Protocol unclassified asText makeAllColor: Color red ]
]

{ #category : 'printing' }
ClyProtocolEditorMorph >> printProtocolOrPackage [

	^self isExtensionActive
		ifTrue: [ ownerTool extendingPackage name]
		ifFalse: [ self printProtocol ]
]

{ #category : 'operations' }
ClyProtocolEditorMorph >> requestChangeBy: aBlock [

	aBlock on: CmdCommandAborted do: [ :err ].

	self update
]

{ #category : 'operations' }
ClyProtocolEditorMorph >> requestPackage [

	| extendingPackage |
	extendingPackage := ownerTool context requestSinglePackage: 'Choose package for method'.

	ownerTool extendingPackage: extendingPackage
]

{ #category : 'operations' }
ClyProtocolEditorMorph >> requestProtocol [

	| newProtocol priorProtocol |
	priorProtocol := ownerTool methodProtocol ifNil: [ '' ].
	newProtocol := StProtocolNameChooserPresenter requestProtocolNameConfiguring: [ :presenter |
		               presenter
			               protocolName: priorProtocol;
			               concernedClass: ownerTool methodClass ].
	newProtocol = priorProtocol ifTrue: [ ^ CmdCommandAborted signal ].

	ownerTool methodProtocol: newProtocol
]

{ #category : 'operations' }
ClyProtocolEditorMorph >> resetProtocolAndPackage [

	ownerTool methodProtocol: Protocol unclassified.
	self update
]

{ #category : 'operations' }
ClyProtocolEditorMorph >> toggleExtension [

	self requestChangeBy: [
		self isExtensionActive
			ifTrue: [ self requestProtocol ]
			ifFalse: [ self requestPackage]
	]
]

{ #category : 'building' }
ClyProtocolEditorMorph >> update [
	label color: label defaultColor.
	label contents: self printProtocolOrPackage.
	self isExtensionActive
		ifTrue: [ editButton helpText: 'Choose package for method']
		ifFalse: [ editButton helpText: 'Classify method']
]
