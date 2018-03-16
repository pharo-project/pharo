"
String morph with enablement support. When disabled the text will appear inset.
"
Class {
	#name : #LabelMorph,
	#superclass : #StringMorph,
	#instVars : [
		'getEnabledSelector',
		'getTextSelector',
		'enabled',
		'model',
		'disabledStyle'
	],
	#category : #'Morphic-Widgets-Basic'
}

{ #category : #examples }
LabelMorph class >> example [
	"self example"
	<sampleInstance>
	<exampleWidget>
	^ (self newLabel: 'A Label') openInWindow
]

{ #category : #examples }
LabelMorph class >> exampleDisable [
	"self exampleDisable"
	<sampleInstance>
	<exampleWidget>
	^ (self newLabel: 'A Label')
		disable;
		openInWindow
]

{ #category : #examples }
LabelMorph class >> exampleDisableInset [
	"self exampleDisable"
	<sampleInstance>
	<exampleWidget>
	^ (self newLabel: 'A Label')
		disable;
		disabledStyle: #inset;
		changeFont;
		openInWindow
]

{ #category : #'instance creation' }
LabelMorph class >> labelFont [
	"in the future should access an instance variable"
		
	^StandardFonts defaultFont
]

{ #category : #'instance creation' }
LabelMorph class >> newLabel: aString [
	"Answer a new text label morph."

	^self
		newLabelFor: nil
		label: aString
		getEnabled: nil
]

{ #category : #'instance creation' }
LabelMorph class >> newLabelFor: aModel label: aString getEnabled: enabledSel [
	"Answer a new text label whose model is aModel"

	^ ( self contents: aString font: self labelFont )
		model: aModel;
		getEnabledSelector: enabledSel
]

{ #category : #accessing }
LabelMorph >> adoptPaneColor: aColor [
]

{ #category : #protocol }
LabelMorph >> disable [
	"Disable the receiver."
	
	self enabled: false
]

{ #category : #accessing }
LabelMorph >> disabledStyle [
	"Answer the value of disabledStyle"

	^ disabledStyle
]

{ #category : #accessing }
LabelMorph >> disabledStyle: anObject [
	"Set the value of disabledStyle"

	disabledStyle := anObject.
	self changed
]

{ #category : #drawing }
LabelMorph >> drawOn: aCanvas [
	"Draw based on enablement."

	super drawOn: aCanvas.

	(self enabled not and: [ self disabledStyle = #inset ])	ifTrue: [
		aCanvas
			drawString: self contents
			in: (self stringBounds translateBy: 1)
			font: self fontToUse
			color: self theme lightBackgroundColor.
	].




		

]

{ #category : #protocol }
LabelMorph >> enable [
	"Enable the receiver."
	
	self enabled: true
]

{ #category : #accessing }
LabelMorph >> enabled [
	"Answer the value of enabled"

	^enabled
]

{ #category : #accessing }
LabelMorph >> enabled: aBoolean [
	"Set the value of enabled"

	enabled == aBoolean ifFalse: [
		enabled := aBoolean.
		self
			changed: #enabled;
			changed]
]

{ #category : #accessing }
LabelMorph >> getEnabledSelector [
	"Answer the value of getEnabledSelector"

	^ getEnabledSelector
]

{ #category : #accessing }
LabelMorph >> getEnabledSelector: anObject [
	"Set the value of getEnabledSelector"

	getEnabledSelector := anObject.
	self updateEnabled
]

{ #category : #accessing }
LabelMorph >> getTextSelector [

	^ getTextSelector
]

{ #category : #accessing }
LabelMorph >> getTextSelector: aSymbol [

	getTextSelector := aSymbol.
	self updateText
]

{ #category : #initialization }
LabelMorph >> initWithContents: aString font: aFont emphasis: emphasisCode [
	"Grrr, why do they do basicNew?"
	
	super initWithContents: aString font: aFont emphasis: emphasisCode.
	self
		disabledStyle: #plain;
		enabled: true
]

{ #category : #initialization }
LabelMorph >> initialize [
	"Initialize the receiver."

	super initialize.
	self
		disabledStyle: #plain;
		enabled: true
]

{ #category : #protocol }
LabelMorph >> interactionState: aSymbol [
	"Backstop here to prevent 'legacy' color handling being applied from
	pluggable buttons."
]

{ #category : #layout }
LabelMorph >> minHeight [
	"Answer the receiver's minimum height.
	based on font height."
	
	^self fontToUse height rounded max: super minHeight
]

{ #category : #layout }
LabelMorph >> minWidth [
	"Answer the minmum width of the receiver.
	Based on font and contents."

	^self valueOfProperty: #minWidth ifAbsent: [self measureContents x] "allow override"
]

{ #category : #accessing }
LabelMorph >> model [
	"Answer the value of model"

	^model
]

{ #category : #accessing }
LabelMorph >> model: anObject [
	"Set my model and make me me a dependent of the given object."

	model ifNotNil: [model removeDependent: self].
	anObject ifNotNil: [anObject addDependent: self].
	model := anObject
]

{ #category : #accessing }
LabelMorph >> stringColor [

	^ self enabled
		ifTrue: [ self color ]
		ifFalse: [ self theme labelDisabledColorFor: self ]
]

{ #category : #updating }
LabelMorph >> update: aSymbol [ 
	"Refer to the comment in View|update:."

	aSymbol == self getEnabledSelector ifTrue:
		[self updateEnabled.
		^ self].
	aSymbol == self getTextSelector ifTrue:
		[self updateText.
		^ self]
]

{ #category : #updating }
LabelMorph >> updateEnabled [
	"Update the enablement state."

	self model ifNotNil: [
		self getEnabledSelector ifNotNil: [
			self enabled: (self model perform: self getEnabledSelector)]]
]

{ #category : #update }
LabelMorph >> updateText [
	"Update the text contents."

	self model ifNotNil: [
		self getTextSelector ifNotNil: [
			self contents: (self model perform: self getTextSelector)]]
]
