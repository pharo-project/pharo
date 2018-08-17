"
The presenter that provides only the standalone form for the form demo
"
Class {
	#name : #SpecDemoStandaloneFormPresenter,
	#superclass : #ComposablePresenterWithModel,
	#instVars : [
		'workingModel',
		'nameLabel',
		'nameTextInput',
		'surnameLabel',
		'surnameTextInput',
		'number1Label',
		'number1Input',
		'number2Label',
		'number2Input',
		'scaleLabel',
		'scaleInput',
		'passwordLabel',
		'passwordInput',
		'checkboxLabel',
		'checkboxInput',
		'dateLabel',
		'dateInput',
		'genderLabel',
		'genderGroup',
		'genderButtons',
		'itemsLabel',
		'itemsInput',
		'submitButton',
		'restoreButton'
	],
	#category : #'Spec-Examples-Demo-Forms'
}

{ #category : #specs }
SpecDemoStandaloneFormPresenter class >> defaultSpec [

	<spec: #default>

	| fontWidth labelsWidth rowHeight checkboxWidth |
	fontWidth := (StandardFonts defaultFont widthOfString: 'M').
	labelsWidth := fontWidth * 8.
	rowHeight := self inputTextHeight.
	checkboxWidth := fontWidth * 2.
	
	^ SpecLayout composed
		newColumn: [ :col | 

			col newRow: [ :row | 
				row add: #nameLabel right: labelsWidth.
				row add: #nameTextInput ]
			height: rowHeight.
		
			col newRow: [ :row | 
				row add: #surnameLabel right: labelsWidth.
				row add: #surnameTextInput ]
			height: rowHeight.

			col newRow: [ :row | 
				row add: #number1Label right: labelsWidth..
				row add: #number1Input ]
			height: rowHeight.

		col newRow: [ :row | 
				row add: #number2Label right: labelsWidth.
				row add: #number2Input ]
			height: rowHeight.

		col newRow: [ :row | 
				row add: #scaleLabel right: labelsWidth.
				row add: #scaleInput ]
			height: rowHeight.

		col newRow: [ :row | 
				row add: #passwordLabel top: 0 bottom: rowHeight left: nil right: labelsWidth.
				row newColumn: [ :passwordColumn |
					passwordColumn newRow: [ :innerRow | 
						innerRow add: #passwordInput].
					passwordColumn newRow: [ :innerRow | 
						innerRow add: #checkboxInput right: checkboxWidth.
						innerRow add: #checkboxLabel ]]]
			height: rowHeight*2.

		col newRow: [ :row | 
				row add: #dateLabel right: labelsWidth.
				row add: #dateInput ]
			height: rowHeight.

	col newRow: [ :row | 
				row add: #genderLabel top: 0 bottom: rowHeight left: nil right: labelsWidth.
				row add: #genderGroup]
			height: rowHeight*2.

	col newRow: [ :row | 
				row add: #itemsLabel right: labelsWidth.
				row add: #itemsInput ]
			height: rowHeight*6.
	
	col newRow: [ :row | 
				row add: #submitButton.
				row add: #restoreButton ]
			height: rowHeight*1.3.

		"fill the rest of the space"
		col newRow: [ :row |  ] ];
			
		yourself.	
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> checkboxInput [
	^ checkboxInput
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> checkboxInput: anObject [
	checkboxInput := anObject
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> checkboxLabel [
	^ checkboxLabel
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> checkboxLabel: anObject [
	checkboxLabel := anObject
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> dateInput [
	^ dateInput
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> dateInput: anObject [
	dateInput := anObject
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> dateLabel [
	^ dateLabel
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> dateLabel: anObject [
	dateLabel := anObject
]

{ #category : #'model updates' }
SpecDemoStandaloneFormPresenter >> fillFormWithWorkingModel [

	| aModel |
	
	aModel := self workingModel.

	self nameTextInput text: aModel name.
	self surnameTextInput text: aModel surname.	
	self number1Input text: aModel number1 asString.
	self number2Input text: aModel number2 asString.
	self scaleInput value: aModel scale.
	self passwordInput text: aModel password.
	self checkboxInput state: aModel rememberMe.
	self dateInput date: aModel date.
	
	genderGroup default: (genderButtons detect: [:each | each key = aModel gender]) value.

	self workingModel items withIndexDo: [ :item :index |
		self itemsInput setSelectionStateFor: index at: (self workingModel selectedItems includes: item) ].
	self itemsInput changed: #listElementAt:
]

{ #category : #'model updates' }
SpecDemoStandaloneFormPresenter >> fillModelWithFormContent [

	self workingModel
		name: self nameTextInput text;
		surname: self surnameTextInput text;
		number1: number1Input text asNumber;
		number2: (NumberParser parse: number2Input text readStream onError: [0]);
		scale: self scaleInput value;
		password: self passwordInput text;
		rememberMe: self checkboxInput state;
		date: self dateInput date.

	 self workingModel gender: (genderButtons detect: [:each | each value = genderGroup currentActivated]) key.
	
	self workingModel selectedItems: self itemsInput selectedItems.

]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> genderGroup [
	^ genderGroup
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> genderGroup: anObject [
	genderGroup := anObject
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> genderLabel [
	^ genderLabel
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> genderLabel: anObject [
	genderLabel := anObject
]

{ #category : #initialization }
SpecDemoStandaloneFormPresenter >> initializePresenter [
	
	self submitButton action: [self submit].	
	self restoreButton action: [self restore].
	
	self number1Input whenTextChanged: [ 
		| receivedText newText |
		receivedText := self number1Input text asString.
		newText := receivedText select: [ :aCharacter | aCharacter isDigit ].
		newText = receivedText ifFalse: [ 
			self number1Input textHolder rawValue: newText.
			self number1Input textHolder valueChanged: newText] ]
]

{ #category : #initialization }
SpecDemoStandaloneFormPresenter >> initializeWidgets [

	nameLabel := self newLabel label: 'name:'.
	nameTextInput := self newTextInput autoAccept: true.

	surnameLabel := self newLabel label: 'surname:'.
	surnameTextInput := self newTextInput autoAccept: true.
	surnameTextInput ghostText: 'empty'.

	number1Label := self newLabel label: 'number 1:'.
	number1Input := self newTextInput autoAccept: true.

	number2Label := self newLabel label: 'number 2:'.
	number2Input := self newTextInput autoAccept: true.

	scaleLabel := self newLabel label: 'scale:'.
	scaleInput := self newSlider.

	passwordLabel := self newLabel label: 'password:'.
	passwordInput := self newTextInput encrypted: true; autoAccept: true..

	checkboxLabel := self newLabel label: 'remember me'.
	checkboxInput := self newCheckBox state: true.

	dateLabel := self newLabel label: 'date:'.
	dateInput := self instantiate: DatePresenter.
	dateInput displayBlock: [:item | item yyyymmdd ].
	
	genderButtons := self model genders collect: [ :each |
		each -> (RadioButtonPresenter new label: each asString) ].  

	genderLabel := self newLabel label: 'gender:'.
	genderGroup := self instantiate: RadioButtonGroupPresenter .
	genderButtons do: [ :each | genderGroup addRadioButton: each value].
	
	itemsLabel := self newLabel label: 'items:'.
	itemsInput := self newList.
	itemsInput beMultipleSelection.
	itemsInput items: self model items.

	submitButton := self newButton label: 'Submit'.
	restoreButton := self newButton label: 'Restore'.
	
	self focusOrder 
		add: nameTextInput;
		add: surnameTextInput;
		add: number1Input;
		add: number2Input;
		add: scaleInput;
		add: passwordInput;
		add: checkboxInput.
		
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> itemsInput [
	^ itemsInput
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> itemsInput: anObject [
	itemsInput := anObject
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> itemsLabel [
	^ itemsLabel
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> itemsLabel: anObject [
	itemsLabel := anObject
]

{ #category : #initialization }
SpecDemoStandaloneFormPresenter >> modelChanged [

	workingModel := self model copy.

	self fillFormWithWorkingModel.

	
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> nameLabel [
	^ nameLabel
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> nameLabel: anObject [
	nameLabel := anObject
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> nameTextInput [
	^ nameTextInput
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> nameTextInput: anObject [
	nameTextInput := anObject
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> number1Input [
	^ number1Input
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> number1Input: anObject [
	number1Input := anObject
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> number1Label [
	^ number1Label
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> number1Label: anObject [
	number1Label := anObject
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> number2Input [
	^ number2Input
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> number2Input: anObject [
	number2Input := anObject
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> number2Label [
	^ number2Label
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> number2Label: anObject [
	number2Label := anObject
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> passwordInput [
	^ passwordInput
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> passwordInput: anObject [
	passwordInput := anObject
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> passwordLabel [
	^ passwordLabel
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> passwordLabel: anObject [
	passwordLabel := anObject
]

{ #category : #actions }
SpecDemoStandaloneFormPresenter >> restore [

	self modelChanged

	
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> restoreButton [
	^ restoreButton
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> restoreButton: anObject [
	restoreButton := anObject
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> scaleInput [
	^ scaleInput
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> scaleInput: anObject [
	scaleInput := anObject
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> scaleLabel [
	^ scaleLabel
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> scaleLabel: anObject [
	scaleLabel := anObject
]

{ #category : #actions }
SpecDemoStandaloneFormPresenter >> submit [

	self fillModelWithFormContent.
	
	self model: workingModel.
	self announcingObject valueChanged.
	
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> submitButton [
	^ submitButton
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> submitButton: anObject [
	submitButton := anObject
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> surnameLabel [
	^ surnameLabel
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> surnameLabel: anObject [
	surnameLabel := anObject
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> surnameTextInput [
	^ surnameTextInput
]

{ #category : #accessing }
SpecDemoStandaloneFormPresenter >> surnameTextInput: anObject [
	surnameTextInput := anObject
]

{ #category : #'accessing - model' }
SpecDemoStandaloneFormPresenter >> workingModel [
	^ workingModel
]

{ #category : #'accessing - model' }
SpecDemoStandaloneFormPresenter >> workingModel: anObject [
	workingModel := anObject
]
