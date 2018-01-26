"
A GLMRubScrolledTextBrick is a wrapper for rubric scrolled text morph, to allow simple integration of rubric text field in brick

Instance Variables
	rubricMorph:		<Object>

rubricMorph
	- xxxxx

"
Class {
	#name : #GLMRubScrolledTextBrick,
	#superclass : #GLMMorphBrick,
	#category : #'Glamour-Morphic-Brick-Widgets-Core'
}

{ #category : #examples }
GLMRubScrolledTextBrick class >> exampleBandOpen [

	self gtExampleBand openInBrickWindow
]

{ #category : #examples }
GLMRubScrolledTextBrick class >> exampleOpen [

	self gtExample openInBrickWindow
]

{ #category : #examples }
GLMRubScrolledTextBrick class >> gtExample [
	<gtExample>
	<label: 'Two rubric text areas filling half of size'>
	
	^ GLMBrick new
		useVerticalLinearLayout;
		padding: #(20 20);
		hSpaceFill;
		vSpaceFill;
		color: Color veryVeryLightGray;
		addAllBricks: { self new vSpaceFill: 50. self new vSpaceFill: 50 };
		yourself
]

{ #category : #examples }
GLMRubScrolledTextBrick class >> gtExampleBand [
	<gtExample>
	<label: 'Two rubric text areas in band'>
	
	^ GLMBandBrick new
		padding: #(20 20);
		hSpaceFill;
		color: Color veryVeryLightGray;
		addAllBricks: { self new vShrinkWrap. self new vShrinkWrap};
		yourself
]

{ #category : #'brick-properties' }
GLMRubScrolledTextBrick >> beTransparent [

	self rubricBackgroundColor: Color transparent
]

{ #category : #'reflective operations' }
GLMRubScrolledTextBrick >> doesNotUnderstand: aMessage [
	^ self rubric
		perform: aMessage selector 
		withArguments: aMessage arguments
]

{ #category : #initialization }
GLMRubScrolledTextBrick >> initialize [
	super initialize.
	
	self rubric: self newRubricMorph.
	self withoutBackground.
	self
		hSpaceFill;
		vSpaceFill
]

{ #category : #'rubric-brick-creation' }
GLMRubScrolledTextBrick >> newRubricMorph [

	^ RubScrolledTextMorph new
]

{ #category : #'brick-properties-events' }
GLMRubScrolledTextBrick >> onThemerChanged [

	self updateStyle
]

{ #category : #'rubric-accessing' }
GLMRubScrolledTextBrick >> rubric [

	^ self morph
]

{ #category : #'rubric-accessing' }
GLMRubScrolledTextBrick >> rubric: aMorph [

	self morph ifNotNil: [ self removeMorph: self morph ].
	self on: aMorph
]

{ #category : #'brick-properties' }
GLMRubScrolledTextBrick >> rubricBackgroundColor [

	^ self valueOfProperty: #brickRubricBackgroundColor ifAbsent: [ self themer rubricBackgroundColor ]
]

{ #category : #'brick-properties' }
GLMRubScrolledTextBrick >> rubricBackgroundColor: aColor [

	self setProperty: #brickRubricBackgroundColor toValue: aColor.
	self updateStyle
]

{ #category : #'brick-properties' }
GLMRubScrolledTextBrick >> rubricBorderColor [

	^ self valueOfProperty: #brickRubricBorderColor ifAbsent: [ self themer rubricBorderColor ]
]

{ #category : #'brick-properties' }
GLMRubScrolledTextBrick >> rubricBorderColor: aColor [

	self setProperty: #brickRubricBorderColor toValue: aColor.
	self updateStyle
]

{ #category : #'brick-properties' }
GLMRubScrolledTextBrick >> rubricBorderWidth [

	^ self valueOfProperty: #brickRubricBorderWidth ifAbsent: [ self themer rubricBorderWidth ]
]

{ #category : #'brick-properties' }
GLMRubScrolledTextBrick >> rubricBorderWidth: aColor [

	self setProperty: #brickRubricBorderWidth toValue: aColor.
	self updateStyle
]

{ #category : #'brick-properties' }
GLMRubScrolledTextBrick >> rubricCursorColor [

	^ self valueOfProperty: #brickRubricCursorColor ifAbsent: [ self themer rubricCursorColor ]
]

{ #category : #'brick-properties' }
GLMRubScrolledTextBrick >> rubricCursorColor: aColor [

	self setProperty: #brickRubricCursorColor toValue: aColor.
	self updateStyle
]

{ #category : #'brick-properties' }
GLMRubScrolledTextBrick >> rubricTextColor [

	^ self valueOfProperty: #brickRubricTextColor ifAbsent: [ self themer rubricTextColor ]
]

{ #category : #'brick-properties' }
GLMRubScrolledTextBrick >> rubricTextColor: aColor [

	self setProperty: #brickRubricTextColor toValue: aColor.
	self updateStyle
]

{ #category : #'brick-properties' }
GLMRubScrolledTextBrick >> rubricTextFamilyName [

	^ self valueOfProperty: #brickRubricTextFamilyName ifAbsent: [ self themer rubricTextFamilyName ]
]

{ #category : #'brick-properties' }
GLMRubScrolledTextBrick >> rubricTextFamilyName: aString [

	self setProperty: #brickRubricTextFamilyName toValue: aString.
	self updateStyle
]

{ #category : #'brick-properties' }
GLMRubScrolledTextBrick >> rubricTextFontSize [

	^ self valueOfProperty: #brickRubricTextFontSize ifAbsent: [ self themer rubricTextFontSize ]
]

{ #category : #'brick-properties' }
GLMRubScrolledTextBrick >> rubricTextFontSize: aFloat [

	self setProperty: #brickRubricTextFontSize toValue: aFloat.
	self updateStyle
]

{ #category : #protocol }
GLMRubScrolledTextBrick >> takeKeyboardFocus [
	self rubric takeKeyboardFocus
]

{ #category : #'brick-properties' }
GLMRubScrolledTextBrick >> text: aText [

	self rubric updateTextWith: aText
]

{ #category : #'rubric-updating' }
GLMRubScrolledTextBrick >> updateStyle [

	self color: self rubricBackgroundColor.
	self rubric borderColor: self rubricBorderColor.
	self rubric borderWidth: self rubricBorderWidth.
	self rubric textColor: self rubricTextColor.
	self rubric font: (LogicalFont familyName: self rubricTextFamilyName pointSize: self rubricTextFontSize).
	self rubric cursor visibleCursorColor: self rubricCursorColor
]

{ #category : #'brick-properties' }
GLMRubScrolledTextBrick >> withoutBackground [

	self rubric color: Color transparent.
	self rubric textArea color: Color transparent
]

{ #category : #'brick-properties' }
GLMRubScrolledTextBrick >> withoutBorder [

	self rubricBorderWidth: 0.
	self rubricBorderColor: Color transparent.
]

{ #category : #'brick-properties' }
GLMRubScrolledTextBrick >> withoutMargins [

	self margins: 0 asMargin
]
