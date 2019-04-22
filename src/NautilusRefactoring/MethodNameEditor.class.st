"
A MethodNameEditor is an editor to set the name of a method.


	MethodNameEditor openOn: (RBMethodName selector: #foo: arguments: #('x')) 
"
Class {
	#name : #MethodNameEditor,
	#superclass : #DialogWindow,
	#instVars : [
		'argumentIndex',
		'labelMorph',
		'methodName',
		'selectorField'
	],
	#category : #'NautilusRefactoring-Utilities'
}

{ #category : #'instance creation' }
MethodNameEditor class >> on: aMethodName [
	^ self basicNew initializeOn: aMethodName
]

{ #category : #'instance creation' }
MethodNameEditor class >> openOn: aMethodName [
	^ UITheme builder openModal: (self on: aMethodName)
]

{ #category : #grips }
MethodNameEditor >> addEdgeGrips [
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
MethodNameEditor >> addGrips [

	self addEdgeGrips
]

{ #category : #accessing }
MethodNameEditor >> argumentIndex [
	^ argumentIndex
]

{ #category : #accessing }
MethodNameEditor >> argumentIndex: anInteger [
	argumentIndex := anInteger.
	self update
]

{ #category : #accessing }
MethodNameEditor >> argumentList [
	^ self methodName arguments
]

{ #category : #actions }
MethodNameEditor >> cancel [
	methodName := nil.
	^ super cancel
]

{ #category : #'focus handling' }
MethodNameEditor >> defaultFocusMorph [
	"Answer the morph that should have the keyboard
	focus by default when the dialog is opened."
	
	^ selectorField
		ifNil: [ super defaultFocusMorph ]
]

{ #category : #actions }
MethodNameEditor >> down [
	self isDownEnabled ifFalse: [ ^ self ].
	self argumentList swap: self argumentIndex with: self argumentIndex + 1.
	self argumentIndex: self argumentIndex + 1
]

{ #category : #initialization }
MethodNameEditor >> initialize [

	super initialize.

	self beResizeable
]

{ #category : #initialization }
MethodNameEditor >> initializeOn: aMethodName [
	methodName := aMethodName.
	argumentIndex := 0.
	self initialize.
	self title: 'Method Name'
]

{ #category : #testing }
MethodNameEditor >> isDownEnabled [
	^ self argumentIndex ~= 0 and: [ self argumentIndex + 1 between: 1 and: self argumentList size ]
]

{ #category : #testing }
MethodNameEditor >> isOkEnabled [
	^ self methodName isValid
]

{ #category : #testing }
MethodNameEditor >> isUpEnabled [
	^ self argumentIndex ~= 0 and: [ self argumentIndex - 1 between: 1 and: self argumentList size ]
]

{ #category : #accessing }
MethodNameEditor >> methodName [
	^ methodName
]

{ #category : #morphic }
MethodNameEditor >> newContentMorph [
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
MethodNameEditor >> newOKButton [
	^ self newOKButtonFor: self getEnabled: #isOkEnabled
]

{ #category : #accessing }
MethodNameEditor >> selector [
	^ self methodName selector
]

{ #category : #accessing }
MethodNameEditor >> selector: aString [
	self methodName selector: aString.
	self update
]

{ #category : #actions }
MethodNameEditor >> up [
	self isUpEnabled ifFalse: [ ^ self ].
	self argumentList swap: self argumentIndex with: self argumentIndex - 1.
	self argumentIndex: self argumentIndex - 1
]

{ #category : #morphic }
MethodNameEditor >> update [
	self changed: #argumentList; changed: #argumentIndex.
	self changed: #isUpEnabled; changed: #isDownEnabled; changed: #isOkEnabled.
	labelMorph contents: self methodName printString
]
