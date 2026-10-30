"
I am the header of the Quick Project Loader. I replace the window title bar, so I also move the window when dragged.
"
Class {
	#name : 'QuickProjectHeaderMorph',
	#superclass : 'Morph',
	#instVars : [
		'dragOffset'
	],
	#category : 'QuickProjectLoader-UI',
	#package : 'QuickProjectLoader',
	#tag : 'UI'
}

{ #category : 'instance creation' }
QuickProjectHeaderMorph class >> title: aString buttons: aCollection [

	^ self new
		title: aString;
		addButtons: aCollection;
		yourself
]

{ #category : 'accessing' }
QuickProjectHeaderMorph >> addButtons: aCollection [

	aCollection do: [ :each | self addMorphBack: each ]
]

{ #category : 'event handling' }
QuickProjectHeaderMorph >> handlesMouseDown: anEvent [

	^ true
]

{ #category : 'initialization' }
QuickProjectHeaderMorph >> initialize [

	super initialize.
	self
		color: Color transparent;
		borderWidth: 0;
		changeTableLayout;
		listDirection: #leftToRight;
		wrapCentering: #center;
		listCentering: #center;
		cellInset: 2;
		layoutInset: (10 @ 4 corner: 6 @ 4);
		hResizing: #spaceFill;
		vResizing: #rigid;
		height: 36
]

{ #category : 'event handling' }
QuickProjectHeaderMorph >> mouseDown: anEvent [

	dragOffset := anEvent position - self window position
]

{ #category : 'event handling' }
QuickProjectHeaderMorph >> mouseMove: anEvent [

	dragOffset ifNotNil: [ self window position: anEvent position - dragOffset ]
]

{ #category : 'event handling' }
QuickProjectHeaderMorph >> mouseUp: anEvent [

	dragOffset := nil
]

{ #category : 'accessing' }
QuickProjectHeaderMorph >> title: aString [

	| label spacer |
	label := StringMorph
		contents: aString
		font: (StandardFonts defaultFont emphasized: TextEmphasis bold emphasisCode).
	label color: self theme textColor.
	spacer := Morph new
		color: Color transparent;
		hResizing: #spaceFill;
		vResizing: #rigid;
		height: 1;
		yourself.
	self
		addMorphBack: label;
		addMorphBack: spacer
]
