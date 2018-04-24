"
A DropListExample is a simple example of how to use drop lists.

self example
"
Class {
	#name : #DropListExample,
	#superclass : #ComposablePresenter,
	#instVars : [
		'container',
		'morph1',
		'morph2',
		'morph3',
		'uniformDropList',
		'heterogeneousDropList',
		'disabledDropList'
	],
	#category : #'Spec-Examples-Morphic'
}

{ #category : #specs }
DropListExample class >> defaultSpec [
	^ SpecColumnLayout composed
		newRow: [ :r | 
			r
				add: #uniformDropList;
				add: #heterogeneousDropList ]
			height: self toolbarHeight;
		newRow: [ :r | r add: #disabledDropList ] height: self toolbarHeight;
		newRow: [ :r | r add: #container ] yourself
]

{ #category : #example }
DropListExample class >> example [
	<sampleInstance>
	
	^ self new openWithSpec
]

{ #category : #specs }
DropListExample class >> title [

	^ 'Drop list'
]

{ #category : #accessing }
DropListExample >> container [

	^ container asSpecAdapter
]

{ #category : #accessing }
DropListExample >> disabledDropList [

	^ disabledDropList
]

{ #category : #accessing }
DropListExample >> heterogeneousDropList [

	^ heterogeneousDropList.
]

{ #category : #initialization }
DropListExample >> initialize [

	container := PanelMorph new.
	self instantiateMorphs.
	
	container
		changeTableLayout;
		listDirection: #bottomToLeft.
	
	super initialize.
]

{ #category : #initialization }
DropListExample >> initializeWidgets [
	uniformDropList := self newDropList.
	heterogeneousDropList := self newDropList.
	(disabledDropList := self newDropList)
		items: #('Disabled' 'Two' 'Three');
		displayBlock: [ :each | each ];
		disable.
	uniformDropList
		items:
			{morph1.
			morph2.
			morph3};
		displayBlock: [ :m | m color name capitalized , ' morph' ];
		iconHolder: [ :e | self iconNamed: #testGreenIcon ];
		whenSelectedItemChanged: [ :m | 
			container removeAllMorphs.
			container addMorph: m ].
	heterogeneousDropList
		addItemLabeled: 'Open workspace'
			do: [ Smalltalk tools workspace open ]
			icon: (self iconNamed: #smallOkIcon);
		addItemLabeled: 'Inspect current morph'
			do: [ uniformDropList selectedItem inspect ]
			icon: (self iconNamed: #testRedIcon).

	"If this is uncommented, it will fire the action of the first item, which is not what we want:
	heterogeneousDropList setSelectedIndex: 1.
	same for:
	heterogeneousDropList setIndex: 1"
	uniformDropList setIndex: 1.
	self setFocus
]

{ #category : #initialization }
DropListExample >> instantiateMorphs [

	morph1 := Morph new
					color: Color red;
					width: 60;
					height: 20.
	morph2 := Morph new
					color: Color blue;
					width: 20;
					height: 60.
	morph3 := Morph new
					color: Color green;
					width: 50;
					height: 50.
]

{ #category : #initialization }
DropListExample >> setFocus [
	
	self focusOrder
		add: uniformDropList;
		add: heterogeneousDropList;
		add: disabledDropList.
	
]

{ #category : #accessing }
DropListExample >> uniformDropList [

	^ uniformDropList
]
