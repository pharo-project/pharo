"
A GLMRubEditingAreaBrick is basic text editor brick used for multi line input fields. 

vShrinkWrap - height fits text size + vertical paddings
hShrinkWrap - doesn't make any sense.
vSpaceFill - fills parent vertically. Actual text editor height = parent height - vertical paddings
hSpaceFill - doesn't really makes sense, since text area still shrinks to fit text
"
Class {
	#name : #GLMRubEditingAreaBrick,
	#superclass : #GLMRubScrolledTextBrick,
	#category : 'Glamour-Morphic-Brick-Widgets-Basic'
}

{ #category : #examples-open }
GLMRubEditingAreaBrick class >> exampleBandOpen [

	self gtExampleBand openInBrickWindow
]

{ #category : #examples-open }
GLMRubEditingAreaBrick class >> exampleFillOpen [

	self gtExampleFill openCenteredInBrickWindow
]

{ #category : #examples-open }
GLMRubEditingAreaBrick class >> exampleOpen [

	self gtExample openCenteredInBrickWindow
]

{ #category : #examples-open }
GLMRubEditingAreaBrick class >> exampleScrollPaneOpen [
	
	self gtExampleScrollPane openInBrickWindowLabeled: 'Rubric in ScrollPane'
]

{ #category : #examples }
GLMRubEditingAreaBrick class >> gtExample [
	<gtExample>
	<label: 'Basic Rubric Editing Area'>
	
	^ self gtExampleBasic
		updateTextWith: 'Hello world!';
		yourself
]

{ #category : #examples }
GLMRubEditingAreaBrick class >> gtExampleBand [
	<gtExample>
	<label: 'Two rubric text areas in band'>
	
	^ GLMBandBrick new
		padding: #(20 20);
		hSpaceFill;
		vShrinkWrap;
		width: 350;
		height: 200;
		color: Color veryVeryLightGray;
		addAllBricks: { self gtExampleBasic. self gtExampleBasic };
		yourself
]

{ #category : #examples }
GLMRubEditingAreaBrick class >> gtExampleBasic [
	<gtExample>
	<label: 'Basic Rubric Editing Area with padding and border'>
	
	^ self new
		padding: 20;
		borderWidth: 20;
		margin: 5;
		width: 300;
		height: 200;
		borderColor: Color veryLightGray
]

{ #category : #examples }
GLMRubEditingAreaBrick class >> gtExampleFill [
	<gtExample>
	<label: 'Basic Rubric Editing Area that fills parent'>
	
	^ self gtExampleBasic
		vSpaceFill;
		updateTextWith: 'Hello world!';
		yourself
]

{ #category : #examples }
GLMRubEditingAreaBrick class >> gtExampleScrollPane [
	<gtExample>
	<label: 'Multiple rubric text areas in scroll pane'>
	
	^ GLMScrollPaneBrick new
		hSpaceFill;
		vSpaceFill;
		color: Color veryVeryLightGray;
		layoutMorphs: true;
		width: 350;
		height: 200;
		addAllBricks: ((1 to: 20) collect: [ :i | self gtExampleBasic ]);
		yourself
]

{ #category : #initialization }
GLMRubEditingAreaBrick >> initialize [
	super initialize.
	
	self
		hSpaceFill;
		vShrinkWrap
]

{ #category : #rubric-brick-creation }
GLMRubEditingAreaBrick >> newRubricMorph [

	^ GLMRubEditingArea new beWrapped
]

{ #category : #brick-geometry }
GLMRubEditingAreaBrick >> shrinkingWidth [

	self rubric width: self rubric textArea width.
	self onLayouted.
	^ self rubric textArea width
]

{ #category : #brick-properties }
GLMRubEditingAreaBrick >> withoutBackground [

	self rubric color: Color transparent
]
