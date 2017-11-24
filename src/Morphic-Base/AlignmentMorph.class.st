"
Used for layout.
Since all morphs now support layoutPolicy the main use of this class is no longer needed.
Kept around for compability. 
Supports a few methods not found elsewhere that can be convenient, eg. newRow

"
Class {
	#name : #AlignmentMorph,
	#superclass : #BorderedMorph,
	#traits : 'TAbleToRotate',
	#classTraits : 'TAbleToRotate classTrait',
	#category : #'Morphic-Base-Basic'
}

{ #category : #example }
AlignmentMorph class >> columnPrototype [
	"Answer a prototypical column"
	"self columnPrototype openInHand"
	<exampleWidget>
	
	| sampleMorphs aColumn |
	sampleMorphs := #(red yellow green) collect:
		[:aColor | Morph new extent: 130 @ 38; color: (Color perform: aColor); setNameTo: aColor asString; yourself].
	aColumn := self inAColumn: sampleMorphs.
	aColumn setNameTo: 'Column'.
	aColumn color: Color veryVeryLightGray.
	aColumn cellInset: 4; layoutInset: 6.
	aColumn enableDragNDrop.
	aColumn setBalloonText: 'Things dropped into here will automatically be organized into a column. Once you have added your own items here, you will want to remove the sample colored rectangles that this started with, and you will want to change this balloon help message to one of your own!'.
	^ aColumn
]

{ #category : #'instance creation' }
AlignmentMorph class >> inAColumn: aCollectionOfMorphs [
	"Answer a columnar AlignmentMorph holding the given collection"

	^ self new inAColumn: aCollectionOfMorphs 
]

{ #category : #'instance creation' }
AlignmentMorph class >> inARow: aCollectionOfMorphs [
	"Answer a row-oriented AlignmentMorph holding the given collection"

	^ self new inARow: aCollectionOfMorphs
]

{ #category : #'instance creation' }
AlignmentMorph class >> newColumn [

	^ self new setAsColumn

]

{ #category : #'instance creation' }
AlignmentMorph class >> newRow [

	^ self new setAsRow
]

{ #category : #'instance creation' }
AlignmentMorph class >> newSpacer: aColor [
	"Answer a space-filling instance of me of the given color."

	^ self new setAsSpacer: aColor
]

{ #category : #'instance creation' }
AlignmentMorph class >> newVariableTransparentSpacer [
	"Answer a space-filling instance of me of the given color."

	^ self new setAsSpacer: Color transparent

]

{ #category : #example }
AlignmentMorph class >> rowPrototype [
	"Answer a prototypical row"
	"self rowPrototype openInHand"
	<exampleWidget>
	
	| sampleMorphs aRow |
	sampleMorphs := (1 to: (2 + 3 atRandom)) collect:
		[:integer | EllipseMorph new extent: ((60 + (20 atRandom)) @ (80 + ((20 atRandom)))); color: Color random; setNameTo: ('egg',  integer asString); yourself].
	aRow := self inARow: sampleMorphs.
	aRow setNameTo: 'Row'.
	aRow enableDragNDrop.
	aRow cellInset: 6.
	aRow layoutInset: 8.
	aRow setBalloonText: 'Things dropped into here will automatically be organized into a row. Once you have added your own items here, you will want to remove the sample colored eggs that this started with, and you will want to change this balloon help message to one of your own!'.
	aRow color: Color veryVeryLightGray.
	^ aRow

			
]

{ #category : #adding }
AlignmentMorph >> addAColumn: aCollectionOfMorphs [

	| col |
	col := self class inAColumn: aCollectionOfMorphs.
	self addMorphBack: col.
	^col
]

{ #category : #adding }
AlignmentMorph >> addARow: aCollectionOfMorphs [

	| row |
	row := self class inARow: aCollectionOfMorphs.
	self addMorphBack: row.
	^row
]

{ #category : #adding }
AlignmentMorph >> addARowCentered: aCollectionOfMorphs [

	^(self addARow: aCollectionOfMorphs)
		hResizing: #shrinkWrap;
		wrapCentering: #center;
		cellPositioning: #leftCenter
]

{ #category : #adding }
AlignmentMorph >> addARowCentered: aCollectionOfMorphs cellInset: cellInsetInteger [

	^(self addARow: aCollectionOfMorphs)
		hResizing: #shrinkWrap;
		wrapCentering: #center;
		cellPositioning: #leftCenter;
		cellInset: cellInsetInteger
]

{ #category : #initialization }
AlignmentMorph >> defaultBorderWidth [
	"answer the default border width for the receiver"
	
	^ 0
]

{ #category : #initialization }
AlignmentMorph >> defaultColor [
	"answer the default color/fill style for the receiver"
	
	^ Color
		r: 0.8
		g: 1.0
		b: 0.8
]

{ #category : #'visual properties' }
AlignmentMorph >> inAColumn: aCollectionOfMorphs [

	self setAsColumn
			color: Color transparent;
			vResizing: #shrinkWrap;
			hResizing: #shrinkWrap;
			layoutInset: 1;
			wrapCentering: #center;
			cellPositioning: #topCenter.
	aCollectionOfMorphs do: [ :each | self addMorphBack: each ].
	^ self 
]

{ #category : #'visual properties' }
AlignmentMorph >> inARightColumn: aCollectionOfMorphs [ 
	
	self setAsColumn 
				 vResizing: #shrinkWrap;
				 layoutInset: 1;
				 wrapCentering: #bottomRight;
				 cellPositioning: #topCenter.
	aCollectionOfMorphs
		do: [ :each | self addMorphBack: each ].
]

{ #category : #'visual properties' }
AlignmentMorph >> inARow: aCollectionOfMorphs [ 

	 self setAsRow 
				color: Color transparent;
				vResizing: #shrinkWrap;
				hResizing: #shrinkWrap;
				layoutInset: 1;
				wrapCentering: #center;
				cellPositioning: #leftCenter.
	aCollectionOfMorphs
		do: [ :each | self addMorphBack: each].

]

{ #category : #initialization }
AlignmentMorph >> initialize [
	
	super initialize.
	self
		layoutPolicy: TableLayout new;
		listDirection: #leftToRight;
	 	wrapCentering: #topLeft;
	 	hResizing: #spaceFill;
	 	vResizing: #spaceFill;
	 	layoutInset: 2;
	 	rubberBandCells: true
	

]

{ #category : #testing }
AlignmentMorph >> isAlignmentMorph [

	^ true

]

{ #category : #opening }
AlignmentMorph >> openInWindowLabeled: aString inWorld: aWorld [

	self layoutInset: 0.
	^super openInWindowLabeled: aString inWorld: aWorld.
]

{ #category : #'visual properties' }
AlignmentMorph >> setAsColumn [

	self 
		listDirection: #topToBottom;
		hResizing: #spaceFill;
		extent: 1@1;
		vResizing: #spaceFill

]

{ #category : #'visual properties' }
AlignmentMorph >> setAsRow [

	self
		listDirection: #leftToRight;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		borderWidth: 0

]

{ #category : #'visual properties' }
AlignmentMorph >> setAsSpacer: aColor [

	self hResizing: #spaceFill;
		vResizing: #spaceFill;
		layoutInset: 0;
		borderWidth: 0;
		extent: 1@1;
		color: aColor.
]

{ #category : #'event handling' }
AlignmentMorph >> wantsKeyboardFocusFor: aSubmorph [
	aSubmorph wouldAcceptKeyboardFocus ifTrue: [^ true].
	^ super wantsKeyboardFocusFor: aSubmorph
]
