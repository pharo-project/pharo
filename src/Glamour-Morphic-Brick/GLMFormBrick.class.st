"
A GLMFormBrick is scriptable html-like form with possibility to submit it.

"
Class {
	#name : #GLMFormBrick,
	#superclass : #GLMBandBrick,
	#category : #'Glamour-Morphic-Brick-Widgets-Basic'
}

{ #category : #examples }
GLMFormBrick class >> exampleOpen [
	<sampleInstance>

	self gtExample
		when: GLMFormSubmitted do: [ :ann | ann result brickAlert ];
		openCenteredInBrickWindow
]

{ #category : #examples }
GLMFormBrick class >> gtExample [
	<gtExample>
	<label: 'Input form with title, two labels, four checkboxes, text and submit button'>
	
	^ self new
		title: [ :t | t
			text: 'Grüezi' ];
		text: [ :t | t
			text: 'Welcome to the Restaurant Zukunft zu Pharo in Berne'];
		label: [ :l | l
			text: 'What would you like to eat?' ];
		checkbox: [ :c | c
			id: #fondue;
			check;
			text: 'Cheese Fondue, CHF 20.-' ];
		checkbox: [ :c | c
			id: #raclette;
			text: 'Raclette, CHF 15.-' ];
		
		label: [ :l | l 
			text: 'What would you like to drink?' ];
		checkbox: [ :c | c
			id: #schorle;
			text: 'Apfel Schorle, CHF 4.-' ];
		checkbox: [ :c | c
			id: #rivella;
			check;
			text: 'Rivella, CHF 4.-' ];
		submit;
		yourself
]

{ #category : #'brick-structure-converting' }
GLMFormBrick >> asFormValue [
	|result|
	result := OrderedCollection new.
	self subbricks do: [ :each |
		each asFormValue ifNotNil: [ :v | result addAll: v asOrderedCollection ] ].
	^ result
]

{ #category : #'form-scripting' }
GLMFormBrick >> checkbox [

	^ self addBrickBack: self newCheckbox
]

{ #category : #'form-scripting' }
GLMFormBrick >> checkbox: aBlock [

	aBlock cull: self checkbox
	
]

{ #category : #initialization }
GLMFormBrick >> initialize [
	super initialize.
	
	self
		hShrinkWrap;
		color: Color veryVeryLightGray muchLighter;
		borderWidth: 1;
		borderColor: Color veryVeryLightGray;
		padding: #(4 7)
]

{ #category : #'form-scripting' }
GLMFormBrick >> label [

	^ self addBrickBack: self newLabel
]

{ #category : #'form-scripting' }
GLMFormBrick >> label: aBlock [

	aBlock cull: self label
	
]

{ #category : #'form-brick-creation' }
GLMFormBrick >> newButton [

	^ GLMButtonBrick new margin: #(3 0)
]

{ #category : #'form-brick-creation' }
GLMFormBrick >> newCheckbox [

	^ GLMCheckboxBrick new margin: #(3 0)
]

{ #category : #'form-brick-creation' }
GLMFormBrick >> newLabel [

	^ GLMLabelBrick new margin: #(3 0)
]

{ #category : #'form-brick-creation' }
GLMFormBrick >> newText [

	^ GLMRubTextAreaBrick new
		width: 200;
		margin: #(3 0);
		vShrinkWrap;
		rubricTextFamilyName: StandardFonts defaultFont familyName
]

{ #category : #'form-scripting' }
GLMFormBrick >> submit [

	^ self addBrickBack: (
		self newButton
			text: 'Submit';
			when: #onClicked do: [ self submitForm ])
]

{ #category : #'form-scripting' }
GLMFormBrick >> submit: aBlock [

	aBlock cull: self submit
]

{ #category : #'form-actions' }
GLMFormBrick >> submitForm [

	self announce: (GLMFormSubmitted new result: self asFormValue)
]

{ #category : #'form-scripting' }
GLMFormBrick >> text [

	^ self addBrickBack: self newText
]

{ #category : #'form-scripting' }
GLMFormBrick >> text: aBlock [

	aBlock cull: self text
]

{ #category : #'form-scripting' }
GLMFormBrick >> title [

	^ self addBrickBack: (self newLabel hAlign: #center)
]

{ #category : #'form-scripting' }
GLMFormBrick >> title: aBlock [

	aBlock cull: self title
]
