"
ColorPresenter that opens a colour selector when clicked.
"
Class {
	#name : #ColorChooserMorph,
	#superclass : #ColorPresenterMorph,
	#traits : 'TEnableOnHaloMenu',
	#classTraits : 'TEnableOnHaloMenu classTrait',
	#instVars : [
		'setColorSelector',
		'enabled',
		'getEnabledSelector'
	],
	#category : #'Morphic-Widgets-ColorPicker'
}

{ #category : #example }
ColorChooserMorph class >> example [
	<sampleInstance>
	^ (ColorChooserMorph 
		on: (BorderedMorph new) 
		color: #color changeColor: #color:) openInWorld
]

{ #category : #'instance creation' }
ColorChooserMorph class >> on: anObject color: getSel changeColor: setSel [
	"Answer a new instance of the receiver on the given model using
	the given selectors as the interface."
	"(ColorChooserMorph on: (BorderedMorph new) color: #color changeColor: #color:) openInWorld"
	^self new
		on: anObject 
		color: getSel
		changeColor: setSel
]

{ #category : #services }
ColorChooserMorph >> chooseColor [
	"Popup the color picker for now."

	|newColor|
	newColor := self theme
		chooseColorIn: ((self ownerThatIsA: SystemWindow) ifNil: [self])
		title: 'Choose Color' translated
		color: self labelMorph color.
	newColor ifNil: [^self].
	self labelMorph color: newColor.
	self solidLabelMorph color: newColor beOpaque.
	self setColorSelector ifNotNil: [self model perform: self setColorSelector with: newColor]
]

{ #category : #accessing }
ColorChooserMorph >> enabled [
	"Answer the enabled state of the receiver."
	
	^enabled
]

{ #category : #accessing }
ColorChooserMorph >> enabled: aBoolean [
	"Set the enabled state of the receiver."

	enabled := aBoolean.
	self contentMorph ifNotNil: [:m | m enabled: aBoolean].
	self changed: #enabled
]

{ #category : #accessing }
ColorChooserMorph >> getEnabledSelector [
	"Answer the value of getEnabledSelector"

	^ getEnabledSelector
]

{ #category : #accessing }
ColorChooserMorph >> getEnabledSelector: anObject [
	"Set the value of getEnabledSelector"

	getEnabledSelector := anObject.
	self updateEnabled
]

{ #category : #initialization }
ColorChooserMorph >> initialize [
	"Initialize the receiver."

	enabled := true.
	super initialize
]

{ #category : #building }
ColorChooserMorph >> newContentMorph [
	"Answer a new button morph"

	|b|
	b := (self theme
		newButtonIn: self
		for: self
		getState: nil
		action: #chooseColor
		arguments: #()
		getEnabled: #enabled
		label: (self newHatchMorph layoutInset: 2)
		help: nil)
		hResizing: #spaceFill.
	b contentHolder hResizing: #spaceFill.
	^b
]

{ #category : #'instance creation' }
ColorChooserMorph >> on: anObject color: getColSel changeColor: setColSel [
	"Set the receiver to the given model parameterized by the given message selectors."

	self
		on: anObject color: getColSel;
		setColorSelector: setColSel
]

{ #category : #accessing }
ColorChooserMorph >> setColorSelector [
	"Answer the value of setColorSelector"

	^ setColorSelector
]

{ #category : #accessing }
ColorChooserMorph >> setColorSelector: anObject [
	"Set the value of setColorSelector"

	setColorSelector := anObject
]

{ #category : #updating }
ColorChooserMorph >> update: aSymbol [ 
	"Refer to the comment in View|update:."

	super update: aSymbol.
	aSymbol == self getEnabledSelector ifTrue: 
		[self updateEnabled.
		^ self]
]

{ #category : #updating }
ColorChooserMorph >> updateEnabled [
	"Update the enablement state."

	self model ifNotNil: [
		self getEnabledSelector ifNotNil: [
			self enabled: (self model perform: self getEnabledSelector)]]
]
