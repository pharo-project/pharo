"
ClassListExample new openOn: Object

"
Class {
	#name : #ClassListExample,
	#superclass : #MorphTreeModel,
	#instVars : [
		'rootClass'
	],
	#category : #Morphic-Examples-MorphTreeWidget
}

{ #category : #examples }
ClassListExample class >> example [

	^self new openOn: self
]

{ #category : #accessing }
ClassListExample >> keyStroke: anEvent from: aTreeMorph [
	self selectedNode
		ifNotNil: [:current | current keyStroke: anEvent from: aTreeMorph]
]

{ #category : #accessing }
ClassListExample >> menu: menu shifted: b [ 
	"Set up the menu to apply to the receiver's, 
	honoring the #shifted boolean"
	super menu: menu shifted: b.
	menu addLine.
	self selectedNode ifNotNil: [:current | current menu: menu shifted: b].
	^ menu
]

{ #category : #'instance creation' }
ClassListExample >> open [
	^ self openOn: Object

]

{ #category : #'instance creation' }
ClassListExample >> openOn: aClass [
	| window |
	self rootClass: aClass.
	window := StandardWindow new model: self.
	window title: aClass name, ' hierarchy'.
	window 
		addMorph: self treeMorph
		fullFrame: LayoutFrame identity.						
	window themeChanged.
	window openInWorld.
	^ window
]

{ #category : #accessing }
ClassListExample >> rootClass [
	^ rootClass ifNil: [rootClass := Object]
]

{ #category : #accessing }
ClassListExample >> rootClass: aClass [
	 rootClass := aClass
]

{ #category : #accessing }
ClassListExample >> rootItems [
	^ self rootClass allSubclasses asArray sort: [:a :b | a name < b name ]
]

{ #category : #accessing }
ClassListExample >> rootNodeClassFromItem: anItem [
	^ClassListNodeExample
]

{ #category : #'user interface' }
ClassListExample >> treeMorph [
	| treeMorph baseColor oddColor evenColor |
	baseColor := Color lightBlue lighter.
	oddColor := (GradientFillStyle
		ramp:
			{(0.0 -> baseColor whiter).
			(0.2 -> baseColor).
			(0.8 -> baseColor darker).
			(1.0 -> baseColor blacker)}) radial: false.
	baseColor := Color veryLightGray muchLighter.
	evenColor := (GradientFillStyle
		ramp:
			{(0.0 -> baseColor lighter lighter).
			(0.2 -> baseColor lighter).
			(0.8 -> baseColor).
			(1.0 -> baseColor blacker)}) radial: false.
	treeMorph := self treeMorphClass new
		model: self;
		beMultiple;
		columns:
			{(MorphTreeColumn new
				startWidth: 100;
				rowMorphGetSelector: #classButton;
				headerButtonLabel: 'Class'
					font: nil
					icon: (self iconNamed: #smallOpenIcon)
					target: nil
					actionSelector: nil
					arguments: #()).
			(MorphTreeColumn new
				rowMorphGetSelector: #commentText;
				headerButtonLabel: 'Comments'
					font: nil
					icon: (self iconNamed: #smallPrintIcon)
					target: nil
					actionSelector: nil
					arguments: #())};
		makeLastColumnUnbounded;
		withHLines: true;
		allowColumnDrop;
		useSquareCorners;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		rowInset: 5;
		columnInset: 5;
		resizerWidth: 2;
		preferedPaneColor: Color white;
		getMenuSelector: #menu:shifted:;
		columnColorForEven: evenColor odd: oddColor.
	^ treeMorph buildContents
]
