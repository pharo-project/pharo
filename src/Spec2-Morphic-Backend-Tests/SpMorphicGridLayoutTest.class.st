Class {
	#name : #SpMorphicGridLayoutTest,
	#superclass : #TestCase,
	#instVars : [
		'label1',
		'morph1',
		'label2',
		'morph2',
		'panel'
	],
	#category : #'Spec2-Morphic-Backend-Tests'
}

{ #category : #private }
SpMorphicGridLayoutTest >> newLayout [
	
	^ SpMorphicGridLayout new
		layout: (SpGridLayout new
			borderWidth: 0;
			columnSpacing: 0;
			rowSpacing: 0;
			columnHomogeneous: false;
			rowHomogeneous: false;
			yourself);
		yourself
]

{ #category : #private }
SpMorphicGridLayoutTest >> place: aMorph at: aPoint [ 

	aMorph 
		setProperty: #gridConstraints 
		toValue: (SpGridConstraints at: aPoint)
]

{ #category : #private }
SpMorphicGridLayoutTest >> place: aMorph at: aPoint span: spanPoint [

	aMorph 
		setProperty: #gridConstraints 
		toValue: (SpGridConstraints at: aPoint span: spanPoint)
]

{ #category : #running }
SpMorphicGridLayoutTest >> setUp [
	
	super setUp.
	label1 := Morph new.
	morph1 := Morph new.
	label2 := Morph new.
	morph2 := Morph new.
	
	panel := Morph new
		extent: 100@100;
		yourself.
		
	{ label1. morph1. label2. morph2 } do: [ :each |
		panel addMorphBack: each.
		self style applyTo: each ]
]

{ #category : #private }
SpMorphicGridLayoutTest >> style [ 

	^ self styleExtent: 50@20
]

{ #category : #private }
SpMorphicGridLayoutTest >> styleExtent: aPoint [
	| rootStyle |

	rootStyle := (SpStyleSTONReader fromString: ('
.application [ Geometry \{ #width: {1}, #height: {2} \} ]'
	format: { aPoint x. aPoint y })).
	
	^ SpMorphStyle newStyles: rootStyle flattenClasses
]

{ #category : #private }
SpMorphicGridLayoutTest >> styleMinExtent: aPoint [
	| rootStyle |

	rootStyle := (SpStyleSTONReader fromString: ('
.application [ Geometry \{ #minWidth: {1}, #minHeight: {2} \} ]'
	format: { aPoint x. aPoint y })).
	
	^ SpMorphStyle newStyles: rootStyle flattenClasses
]

{ #category : #tests }
SpMorphicGridLayoutTest >> testBasicLayout [
	"
	-----------
	| 50 | 50 |
	-----------
	| 50 | 50 |
	-----------
	"
	| layout |
	
	self place: label1 at: 1@1.
	self place: morph1 at: 2@1.
	self place: label2 at: 1@2.
	self place: morph2 at: 2@2.
	
	layout := self newLayout.
	layout layout: panel in: (0@0 corner: 100@100).
	
	self assert: label1 bounds equals: (0@0 corner: 50@20).
	self assert: morph1 bounds equals: (50@0 corner: 100@20).
	self assert: label2 bounds equals: (0@20 corner: 50@40).
	self assert: morph2 bounds equals: (50@20 corner: 100@40)
]

{ #category : #tests }
SpMorphicGridLayoutTest >> testBorderWidthHomogeneous [
	"
	+---+----+----+---+
	| 5 | 5  | 5  | 5 |
	+---+----+----+---+
	| 5 | 45 | 45 | 5 |
	+---+----+----+---+
	| 5 | 45 | 45 | 5 |
	+---+----+----+---+
	| 5 | 5  | 5  | 5 |
	+---+----+----+---+
	"
	| layout |
	
	self place: label1 at: 1@1.
	self place: morph1 at: 2@1.
	self place: label2 at: 1@2.
	self place: morph2 at: 2@2.
	
	(layout := self newLayout) layout 
		beColumnHomogeneous;
		beRowHomogeneous;
		borderWidth: 5.
	layout layout: panel in: (0@0 corner: 100@100).
	
	self assert: label1 bounds equals: (5@5 corner: 50@50).
	self assert: morph1 bounds equals: (50@5 corner: 95@50).
	self assert: label2 bounds equals: (5@50 corner: 50@95).
	self assert: morph2 bounds equals: (50@50 corner: 95@95)
]

{ #category : #tests }
SpMorphicGridLayoutTest >> testBorderWidthNotHomogeneous [
	"
	+---+----+----+
	| 5 | 5  | 5  |
	+---+----+----+
	| 5 | 50 | 50 |
	+---+----+----+
	| 5 | 50 | 50 | 
	+---+----+----+
	"
	| layout |
	
	self place: label1 at: 1@1.
	self place: morph1 at: 2@1.
	self place: label2 at: 1@2.
	self place: morph2 at: 2@2.
	
	(layout := self newLayout) layout
		beRowNotHomogeneous; 
		beColumnNotHomogeneous;
		borderWidth: 5.
	layout layout: panel in: (0@0 corner: 100@100).
	
	self assert: label1 bounds equals: (5@5 corner: 55@25).
	self assert: morph1 bounds equals: (55@5 corner: 105@25).
	self assert: label2 bounds equals: (5@25 corner: 55@45).
	self assert: morph2 bounds equals: (55@25 corner: 105@45)
]

{ #category : #tests }
SpMorphicGridLayoutTest >> testColumnHomogeneousDiferentRowsAndColumns [
	"Each element in a different row and column
	
	-------------------------
	| 100 |     |     |     |
	-------------------------
	|     | 100 |     |     |
	-------------------------
	|     |     | 100 |     |
	-------------------------
	|     |     |     | 100 |
	-------------------------	
	"
	| layout |
	
	self place: label1 at: 1@1.
	self place: morph1 at: 2@2.
	self place: label2 at: 3@3.
	self place: morph2 at: 4@4.
			
	layout := self newLayout.
	layout layout beColumnHomogeneous.
	"make one column width to 100 (it should force all columns to 100, instead 25)"
	(self styleMinExtent: 100@20) applyTo: label2.
	
	layout layout: panel in: (0@0 corner: 100@100).
	
	self assert: label1 bounds equals: (0@0 corner: 100@20).
	self assert: morph1 bounds equals: (100@20 corner: 200@40).	
	self assert: label2 bounds equals: (200@40 corner: 300@60).
	self assert: morph2 bounds equals: (300@60 corner: 400@80)

]

{ #category : #tests }
SpMorphicGridLayoutTest >> testColumnNotHomogeneousDiferentRowsAndColumns [
	"Each element in a different row and column
	
	-------------------------
	| 30  |     |     |     |
	-------------------------
	|     | 40  |     |     |
	-------------------------
	|     |     | 50  |     |
	-------------------------
	|     |     |     | 60  |
	-------------------------	
	"
	| layout |
	
	self place: label1 at: 1@1.
	self place: morph1 at: 2@2.
	self place: label2 at: 3@3.
	self place: morph2 at: 4@4.
	
	layout := self newLayout.
	layout layout beColumnNotHomogeneous.
	"make columns differ, to make each column different"
	(self styleExtent: 30@20) applyTo: label1.
	(self styleExtent: 40@20) applyTo: morph1.
	(self styleExtent: 50@20) applyTo: label2.
	(self styleExtent: 60@20) applyTo: morph2.
	
	layout layout: panel in: (0@0 corner: 100@100).
	
	self assert: label1 bounds equals: (0@0 corner: 30@20).
	self assert: morph1 bounds equals: (30@20 corner: 70@40).	
	self assert: label2 bounds equals: (70@40 corner: 120@60).
	self assert: morph2 bounds equals: (120@60 corner: 180@80)

]

{ #category : #tests }
SpMorphicGridLayoutTest >> testColumnNotHomogeneousPairedRowsAndColumns [
	"Paired elements in rows and columns
	
	-----------
	| 50 | 60 |
	-----------
	| 50 | 60 |
	-----------
	
	"
	| layout |
	
	self place: label1 at: 1@1.
	self place: morph1 at: 2@1.
	self place: label2 at: 1@2.
	self place: morph2 at: 2@2.
	
	layout := self newLayout.
	layout layout beColumnNotHomogeneous.
	"make columns differ, to make each column different"
	(self styleExtent: 30@20) applyTo: label1.
	(self styleExtent: 50@20) applyTo: morph1.
	(self styleExtent: 40@20) applyTo: label2.
	(self styleExtent: 60@20) applyTo: morph2.
	
	layout layout: panel in: (0@0 corner: 100@100).
	
	self assert: label1 bounds equals: (0@0 corner: 40@20).
	self assert: morph1 bounds equals: (40@0 corner: 100@20).	
	self assert: label2 bounds equals: (0@20 corner: 40@40).
	self assert: morph2 bounds equals: (40@20 corner: 100@40)

]

{ #category : #tests }
SpMorphicGridLayoutTest >> testColumnSpacing [
	"
	---------------
	| 50 | 5 | 50 |
	---------------
	| 50 | 5 | 50 |
	---------------
	"
	| layout |
	
	self place: label1 at: 1@1.
	self place: morph1 at: 2@1.
	self place: label2 at: 1@2.
	self place: morph2 at: 2@2.
	
	(layout := self newLayout) layout columnSpacing: 5.
	layout layout: panel in: (0@0 corner: 100@100).
	
	self assert: label1 bounds equals: (0@0 corner: 50@20).
	self assert: morph1 bounds equals: (55@0 corner: 105@20).
	self assert: label2 bounds equals: (0@20 corner: 50@40).
	self assert: morph2 bounds equals: (55@20 corner: 105@40)
]

{ #category : #tests }
SpMorphicGridLayoutTest >> testLayoutIn [
	"This is a meta-test that tries all the others (just to have access to all tests while seeing 
	 the class. Yes, I know that means all tests are executed twice in CI, but they are fast :)"

	(self class localSelectors 
		reject: [ :each | each = #testLayoutIn ])
		select: [ :each | each beginsWith: 'test' ]
		thenDo: [ :each |
			self setUp.
			self perform: each.
			self tearDown ]
]

{ #category : #tests }
SpMorphicGridLayoutTest >> testRowSpacing [
	"
	-----------
	| 50 | 50 |
	-----------
	| 5  | 5  |
	-----------
	| 50 | 50 |
	-----------
	"
	| layout |
	
	self place: label1 at: 1@1.
	self place: morph1 at: 2@1.
	self place: label2 at: 1@2.
	self place: morph2 at: 2@2.
	
	(layout := self newLayout) layout rowSpacing: 5.
	layout layout: panel in: (0@0 corner: 100@100).
	
	self assert: label1 bounds equals: (0@0 corner: 50@20).
	self assert: morph1 bounds equals: (50@0 corner: 100@20).
	self assert: label2 bounds equals: (0@25 corner: 50@45).
	self assert: morph2 bounds equals: (50@25 corner: 100@45)
]

{ #category : #tests }
SpMorphicGridLayoutTest >> testSpanColumn [
	"Each element in a different row and column
	
	--------------------
	| 50  |  50  | 50  | 
	--------------------
	|     |        100 | 
	--------------------
	"
	| layout |
	
	self place: label1 at: 1@1.
	self place: morph1 at: 2@1.
	self place: label2 at: 3@1.
	self place: morph2 at: 2@2 span: 2@1.
	
	layout := self newLayout.
	layout layout: panel in: (0@0 corner: 100@100).
	
	self assert: label1 bounds equals: (0@0 corner: 50@20).
	self assert: morph1 bounds equals: (50@0 corner: 100@20).	
	self assert: label2 bounds equals: (100@0 corner: 150@20).
	self assert: morph2 bounds equals: (50@20 corner: 150@40)

]

{ #category : #tests }
SpMorphicGridLayoutTest >> testSpanRow [
	"Each element in a different row and column
	
	--------------------
	| 50  |      |     | 
	-------  50  -------
	| 50  |      | 50  | 
	--------------------
	"
	| layout |
	
	self place: label1 at: 1@1.
	self place: morph1 at: 2@1 span: 1@2.
	self place: label2 at: 1@2.
	self place: morph2 at: 3@2.
	
	layout := self newLayout.
	layout layout: panel in: (0@0 corner: 100@100).
	
	self assert: label1 bounds equals: (0@0 corner: 50@20).
	self assert: morph1 bounds equals: (50@0 corner: 100@40).	
	self assert: label2 bounds equals: (0@20 corner: 50@40).
	self assert: morph2 bounds equals: (100@20 corner: 150@40)
]
