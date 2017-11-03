"
A GLMButtonBrick is a basic  button with label and icon to the left of the label

Instance Variables
	iconBrick:		<GLMInteractableBrick>
	labelBrick:		<GLMLabelBrick>

iconBrick
	- an icon to be displayed to the left of the label

labelBrick
	- label showing button text

"
Class {
	#name : #GLMButtonBrick,
	#superclass : #GLMInteractableBrick,
	#instVars : [
		'iconBrick',
		'labelBrick'
	],
	#category : #Glamour-Morphic-Brick-Widgets-Basic
}

{ #category : #examples }
GLMButtonBrick class >> exampleOpen [

	self gtExample openCenteredInBrickWindow
]

{ #category : #examples }
GLMButtonBrick class >> gtExample [
	<gtExample>
	<label: 'Simple button'>
	
	^ self new
]

{ #category : #brick-interactable-actions }
GLMButtonBrick >> click: evt [

	super click: evt.
	^ true
]

{ #category : #brick-layout-properties }
GLMButtonBrick >> hShrinkWrap [
	super hShrinkWrap.
	
	self label hShrinkWrap
]

{ #category : #brick-layout-properties }
GLMButtonBrick >> hSpaceFill: anInteger [
	super hSpaceFill: anInteger.
	
	self label hSpaceFill.
]

{ #category : #button-brick-accessing }
GLMButtonBrick >> iconBrick [

	^ iconBrick
]

{ #category : #button-brick-accessing }
GLMButtonBrick >> iconBrick: anObject [

	iconBrick := anObject
]

{ #category : #initialization }
GLMButtonBrick >> initialize [
	super initialize.
	
	self
		enable;
		labelBrick: self newLabelBrick;
		hShrinkWrap;
		vShrinkWrap
]

{ #category : #button-brick-accessing }
GLMButtonBrick >> label [

	^ labelBrick
]

{ #category : #button-brick-accessing }
GLMButtonBrick >> labelBrick: anObject [
	labelBrick := anObject.
	
	self label
		vShrinkWrap;
		hShrinkWrap;
		text: 'button';
		textPosition: #center;
		vAlign: #center;
		hAlign: #center.
	
	self addBrickBack: self label
]

{ #category : #button-brick-creation }
GLMButtonBrick >> newLabelBrick [

	^ GLMLabelBrick new
]

{ #category : #brick-properties-events }
GLMButtonBrick >> onThemerChanged [
	super onThemerChanged.

	self updateStyle
]

{ #category : #button-properties }
GLMButtonBrick >> text [

	^ self label text
]

{ #category : #button-properties }
GLMButtonBrick >> text: aString [
	"changes button text, redraws and updates button bounds"
	self label text: aString
]

{ #category : #button-updating }
GLMButtonBrick >> updateStyle [

	self borderWidth: self themer buttonBorderWidth.
	self borderColor: self themer buttonBorderColor.
	self padding: self themer buttonPadding.
	self normalColor: self themer buttonBackgroundColor.
	self selectedColor: self themer buttonSelectedColor.
	self pressedColor: self themer buttonPressedColor.
	self minWidth: self themer buttonMinWidth.
	self minHeight: self themer buttonMinHeight.
	self doLayoutForce
	
]

{ #category : #brick-layout-properties }
GLMButtonBrick >> vShrinkWrap [
	super vShrinkWrap.
	
	self label vShrinkWrap
]

{ #category : #brick-layout-properties }
GLMButtonBrick >> vSpaceFill: anInteger [
	super vSpaceFill: anInteger.
	
	self label vSpaceFill.
]
