Class {
	#name : #SpGridLayoutExample,
	#superclass : #SpPresenter,
	#instVars : [
		'element11',
		'element21',
		'element12',
		'element22',
		'borderWidth',
		'columnSpacing',
		'rowSpacing',
		'columnHomogeneous',
		'rowHomogeneous'
	],
	#category : #'Spec2-Examples-Layouts'
}

{ #category : #specs }
SpGridLayoutExample class >> defaultSpec [

	^ SpBoxLayout newVertical
		add: (SpGridLayout new 
			beColumnHomogeneous;
			beRowHomogeneous;
			add: #element11 at: 1@1;
			add: #element21 at: 2@1;
			add: #element12 at: 1@2;
			add: #element22 at: 2@2;
			yourself);
		add: (SpGridLayout new
			beColumnHomogeneous;
			add: 'Border Width' 			at: 1@1; add: #borderWidth 		at: 2@1;
			add: 'Column Spacing'			at: 1@2; add: #columnSpacing 	at: 2@2;
			add: 'Row spacing'				at: 1@3; add: #rowSpacing 		at: 2@3;
			add: #columnHomogeneous 		at: 1@4 span: 2@1;
			add: #rowHomogeneous 			at: 1@5 span: 2@1;
			yourself);
		yourself
]

{ #category : #'instance creation' }
SpGridLayoutExample class >> open [ 
	<script>

	self new openWithSpec
]

{ #category : #'instance creation' }
SpGridLayoutExample class >> openWithGtk [
	<script>
	
	GtkApplication ensureRunning.
	(SpApplication new 
		useBackend: #Gtk;
		new: self)
		openWithSpec
]

{ #category : #initialization }
SpGridLayoutExample >> initializeWidgets [

	"example elements"
	element11 := self newMorph morph: (Morph new color: Color red).
	element21 := self newMorph morph: (Morph new color: Color green).
	element12 := self newMorph morph: (Morph new color: Color blue).
	element22 := self newMorph morph: (Morph new color: Color yellow).
	"element11 := self newLabel label: '1x1'.	
	element21 := self newLabel label: '2x1'.	
	element12 := self newLabel label: '1x2'.	
	element22 := self newLabel label: '2x2'."	

	"control elements"
	borderWidth := self newSlider beHorizontal; min: 0; max: 100.
	columnSpacing := self newSlider beHorizontal; min: 0; max: 100.
	rowSpacing := self newSlider beHorizontal; min: 0; max: 100.
	columnHomogeneous := self newCheckBox
		label: 'Column is homogeneous'.
	rowHomogeneous := self newCheckBox 
		label: 'Row is homogeneous'.
		
	self announcer 
		when: SpWidgetBuilt 
		send: #widgetBuilt:
		to: self
]

{ #category : #initialization }
SpGridLayoutExample >> widgetBuilt: ann [
	| exampleLayout |
	
	exampleLayout := self layout presenters first.
	
	borderWidth value: exampleLayout borderWidth.
	columnSpacing value: exampleLayout columnSpacing.
	rowSpacing value: exampleLayout rowSpacing.
	columnHomogeneous state: exampleLayout isColumnHomogeneous.
	rowHomogeneous state: exampleLayout isRowHomogeneous
]
