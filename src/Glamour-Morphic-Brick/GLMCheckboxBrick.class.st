"
A GLMCheckboxBrick is very simple checkbox with label support. If label text is set it becomes clickable too.

Instance Variables
	checkBrick:		<GLMInteractableBrick>
	labelBrick:		<GLMLabelBrick>

checkBrick
	- checkbox itself

labelBrick
	- label position to the right from checkbox

"
Class {
	#name : #GLMCheckboxBrick,
	#superclass : #GLMInteractableBrick,
	#instVars : [
		'labelBrick',
		'checkBrick',
		'id'
	],
	#category : #'Glamour-Morphic-Brick-Widgets-Basic'
}

{ #category : #examples }
GLMCheckboxBrick class >> exampleOpen [

	self gtExample openCenteredInBrickWindow
]

{ #category : #examples }
GLMCheckboxBrick class >> exampleWithLabelOpen [

	self gtExampleWithLabel openCenteredInBrickWindow
]

{ #category : #examples }
GLMCheckboxBrick class >> gtExample [
	<gtExample>
	<label: 'Checkbox without label'>
	
	^ self new
]

{ #category : #examples }
GLMCheckboxBrick class >> gtExampleWithLabel [
	<gtExample>
	<label: 'Checkbox with label'>
	
	^ self new
		text: 'I''m label, click me';
		action: [ :boolean | self inform: 'Checkbox set to ', boolean asString ];
		yourself
]

{ #category : #'checkbox-accessing' }
GLMCheckboxBrick >> action: aBlockClosure [ 
	"aBlockClosure can receive as the first argument aBoolean, as the second argument this object."
	self 
		when: #onChecked 
			do: [ :anAnnouncement :anAnnouncer | 
					aBlockClosure cull: true cull: self cull: anAnnouncement cull: anAnnouncer ];
		when: #onUnchecked
			do: [ :anAnnouncement :anAnnouncer | 
					aBlockClosure cull: false cull: self cull: anAnnouncement cull: anAnnouncer ]
]

{ #category : #'brick-structure-converting' }
GLMCheckboxBrick >> asFormValue [

	^ self id -> self isChecked
]

{ #category : #'checkbox-brick-accessing' }
GLMCheckboxBrick >> box [

	^ checkBrick
]

{ #category : #'brick-interactable-actions' }
GLMCheckboxBrick >> check [
	self box check.
	self label check.
	super check
]

{ #category : #'checkbox-brick-accessing' }
GLMCheckboxBrick >> checkBrick: anObject [
	checkBrick := anObject.
	self box
		borderColor: self themer checkboxBorderColor;
		borderWidth: self themer checkboxBorderWidth;
		vAlign: #center;
		checkedImage: self themer checkboxCheckedImage;
		width: self themer checkboxWidth;
		height: self themer checkboxHeight.
		
	self addBrickFirst: self box
]

{ #category : #'brick-interactable-actions' }
GLMCheckboxBrick >> click: evt [

	self toggle.
	^ true
]

{ #category : #'brick-interactable-actions' }
GLMCheckboxBrick >> deselect: anEvent [
	super deselect: anEvent.
	self box deselect: anEvent.
	self label deselect: anEvent
]

{ #category : #'brick-layout-properties' }
GLMCheckboxBrick >> hShrinkWrap [
	super hShrinkWrap.
	
	self label hShrinkWrap
]

{ #category : #'brick-layout-properties' }
GLMCheckboxBrick >> hSpaceFill: anInteger [
	super hSpaceFill: anInteger.
	
	self label hSpaceFill
]

{ #category : #'checkbox-accessing' }
GLMCheckboxBrick >> id [

	^ id ifNil: [ self ]
]

{ #category : #'checkbox-scripting' }
GLMCheckboxBrick >> id: anObejct [

	id := anObejct
]

{ #category : #initialization }
GLMCheckboxBrick >> initialize [
	super initialize.
	
	self
		checkBrick: self newCheckboxBrick;
		labelBrick: self newLabelBrick;
		useHorizontalLinearLayout;
		vShrinkWrap;
		hShrinkWrap;
		enable
]

{ #category : #'checkbox-brick-accessing' }
GLMCheckboxBrick >> label [

	^ labelBrick
]

{ #category : #'checkbox-brick-accessing' }
GLMCheckboxBrick >> labelBrick: anObject [
	labelBrick := anObject.
	
	labelBrick
		vAlign: #center;
		marginLeft: 5;
		textPosition: #leftCenter.
]

{ #category : #'checkbox-brick-creation' }
GLMCheckboxBrick >> newCheckboxBrick [

	^ GLMInteractableBrick new
]

{ #category : #'checkbox-brick-creation' }
GLMCheckboxBrick >> newLabelBrick [

	^ GLMLabelBrick new
]

{ #category : #'brick-interactable-actions' }
GLMCheckboxBrick >> select: anEvent [
	super select: anEvent.
	self box select: anEvent.
	self label select: anEvent
]

{ #category : #'checkbox-scripting' }
GLMCheckboxBrick >> text: aString [

	self label text: aString.
	self label text isEmpty
			ifTrue: [ self removeBrick: self label ]
			ifFalse: [ (self hasSubbrick: self label)
				ifFalse: [ self addBrickBack: self label ] ].
	self doLayoutForce
]

{ #category : #'checkbox-actions' }
GLMCheckboxBrick >> toggle [

	self isChecked
		ifTrue: [ self uncheck ]
		ifFalse: [ self check ]
]

{ #category : #'brick-interactable-actions' }
GLMCheckboxBrick >> uncheck [
	self box uncheck.
	self label uncheck.
	super uncheck
]

{ #category : #'brick-layout-properties' }
GLMCheckboxBrick >> vShrinkWrap [
	super vShrinkWrap.
	
	self label vShrinkWrap
]

{ #category : #'brick-layout-properties' }
GLMCheckboxBrick >> vSpaceFill: anInteger [
	super vSpaceFill: anInteger.
	
	self label vSpaceFill.
]
