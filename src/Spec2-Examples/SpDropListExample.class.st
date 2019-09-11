"
A DropListExample is a simple example of how to use drop lists.

self example
"
Class {
	#name : #SpDropListExample,
	#superclass : #SpPresenter,
	#instVars : [
		'container',
		'morph1',
		'morph2',
		'morph3',
		'uniformDropList',
		'heterogeneousDropList',
		'disabledDropList'
	],
	#category : #'Spec2-Examples-Morphic'
}

{ #category : #specs }
SpDropListExample class >> defaultSpec [
	^ SpColumnLayout composed
		newRow: [ :r | 
			r
				add: #uniformDropList;
				add: #heterogeneousDropList ]
			height: self toolbarHeight;
		newRow: [ :r | r add: #disabledDropList ] height: self toolbarHeight;
		newRow: [ :r | r add: #container ] yourself
]

{ #category : #example }
SpDropListExample class >> example [
	<sampleInstance>
	
	^ self new openWithSpec
]

{ #category : #specs }
SpDropListExample class >> title [

	^ 'Drop list'
]

{ #category : #accessing }
SpDropListExample >> container [

	^ container asSpAdapter
]

{ #category : #accessing }
SpDropListExample >> disabledDropList [

	^ disabledDropList
]

{ #category : #accessing }
SpDropListExample >> heterogeneousDropList [

	^ heterogeneousDropList.
]

{ #category : #initialization }
SpDropListExample >> initialize [

	container := PanelMorph new.
	self instantiateMorphs.
	
	container
		changeTableLayout;
		listDirection: #bottomToLeft.
	
	super initialize.
]

{ #category : #initialization }
SpDropListExample >> initializeWidgets [
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
		iconBlock: [ :e | self iconNamed: #testGreenIcon ];
		whenSelectedItemChangedDo: [ :m | 
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
	uniformDropList selectedIndex: 1.
	self setFocus
]

{ #category : #initialization }
SpDropListExample >> instantiateMorphs [

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
SpDropListExample >> setFocus [
	
	self focusOrder
		add: uniformDropList;
		add: heterogeneousDropList;
		add: disabledDropList.
	
]

{ #category : #accessing }
SpDropListExample >> uniformDropList [

	^ uniformDropList
]
