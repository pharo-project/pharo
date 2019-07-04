"
I am an editor to set the name of a method.
I was copied from OB.

I should be created on the instance of RBMethodName: 

	methodName := RBMethodName selector: selector arguments: argumentNames copy.
	dialog := SycMethodNameEditor openOn: methodName.

I modify given method name instance. When dialog is submitted you can ask new properties: 

	methodName selector.
	methodName arguments

Internal Representation and Key Implementation Points.

    Instance Variables
	argumentIndex:		<Integer>
	labelMorph:		<StringMorph>
	methodName:		<RBMethodName>
	selectorField:		<Object>
"
Class {
	#name : #SycMethodNameEditor,
	#superclass : #DialogWindow,
	#instVars : [
		'argumentIndex',
		'labelMorph',
		'methodName',
		'selectorField'
	],
	#category : #'SystemCommands-RefactoringSupport'
}

{ #category : #'instance creation' }
SycMethodNameEditor class >> on: aMethodName [
	^ self basicNew initializeOn: aMethodName
]

{ #category : #'instance creation' }
SycMethodNameEditor class >> openOn: aMethodName [
	^ UITheme builder openModal: (self on: aMethodName)
]

{ #category : #grips }
SycMethodNameEditor >> addEdgeGrips [
	"Should add these to the front!"
	
	| l r lh |
	lh := self labelHeight.
	l  := WindowEdgeGripMorph new target: self; position: self position; edgeName: #left.
	l layoutFrame topOffset: lh negated + 22.
	r  := WindowEdgeGripMorph new target: self; position: self position; edgeName: #right.
	r layoutFrame topOffset: lh negated + 22.
	self
		addMorph: l;
		addMorph: r

]

{ #category : #grips }
SycMethodNameEditor >> addGrips [

	self addEdgeGrips
]

{ #category : #accessing }
SycMethodNameEditor >> argumentIndex [
	^ argumentIndex
]

{ #category : #accessing }
SycMethodNameEditor >> argumentIndex: anInteger [
	argumentIndex := anInteger.
	self update
]

{ #category : #accessing }
SycMethodNameEditor >> argumentList [
	^ self methodName arguments
]

{ #category : #actions }
SycMethodNameEditor >> cancel [
	methodName := nil.
	^ super cancel
]

{ #category : #'focus handling' }
SycMethodNameEditor >> defaultFocusMorph [
	"Answer the morph that should have the keyboard
	focus by default when the dialog is opened."
	
	^ selectorField
		ifNil: [ super defaultFocusMorph ]
]

{ #category : #actions }
SycMethodNameEditor >> down [
	self isDownEnabled ifFalse: [ ^ self ].
	self argumentList swap: self argumentIndex with: self argumentIndex + 1.
	self argumentIndex: self argumentIndex + 1
]

{ #category : #initialization }
SycMethodNameEditor >> initialize [

	super initialize.

	self beResizeable
]

{ #category : #initialization }
SycMethodNameEditor >> initializeOn: aMethodName [
	methodName := aMethodName.
	argumentIndex := 0.
	self initialize.
	self title: 'Method Name'
]

{ #category : #testing }
SycMethodNameEditor >> isDownEnabled [
	^ self argumentIndex ~= 0 and: [ self argumentIndex + 1 between: 1 and: self argumentList size ]
]

{ #category : #testing }
SycMethodNameEditor >> isOkEnabled [
	^ self methodName isValid
]

{ #category : #testing }
SycMethodNameEditor >> isUpEnabled [
	^ self argumentIndex ~= 0 and: [ self argumentIndex - 1 between: 1 and: self argumentList size ]
]

{ #category : #accessing }
SycMethodNameEditor >> methodName [
	^ methodName
]

{ #category : #morphic }
SycMethodNameEditor >> newContentMorph [
	^ (self newRow: {
		self newLabelGroup: {
			'Selector:' -> (selectorField:=(self newTextEntryFor: self getText: #selector setText: #selector: help: nil)
				autoAccept: true;
				on: #keyStroke send: #value:value: to: [:key :morph | key keyCharacter = Character cr ifTrue: [ self ok. true]. false];
				yourself).
			'Arguments:' -> (self newRow: {
				(self newListFor: self list: #argumentList selected: #argumentIndex changeSelected: #argumentIndex: help: nil)
					hResizing: #spaceFill;
					yourself.
				(self newColumn: {
					(self newButtonFor: self action: #up label: 'up'  help: nil)
						getEnabledSelector: #isUpEnabled.
					(self newButtonFor: self action: #down label: 'dn'  help: nil)
						getEnabledSelector: #isDownEnabled })
					hResizing: #shrinkWrap }).
			'Preview:' -> (labelMorph := self newLabel: self methodName printString) } })
 		minWidth: 400;
		yourself
]

{ #category : #morphic }
SycMethodNameEditor >> newOKButton [
	^ self newOKButtonFor: self getEnabled: #isOkEnabled
]

{ #category : #accessing }
SycMethodNameEditor >> selector [
	^ self methodName selector
]

{ #category : #accessing }
SycMethodNameEditor >> selector: aString [
	self methodName selector: aString.
	self update
]

{ #category : #actions }
SycMethodNameEditor >> up [
	self isUpEnabled ifFalse: [ ^ self ].
	self argumentList swap: self argumentIndex with: self argumentIndex - 1.
	self argumentIndex: self argumentIndex - 1
]

{ #category : #morphic }
SycMethodNameEditor >> update [
	self changed: #argumentList; changed: #argumentIndex.
	self changed: #isUpEnabled; changed: #isDownEnabled; changed: #isOkEnabled.
	labelMorph contents: self methodName printString
]
