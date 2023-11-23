Class {
	#name : 'TableLayoutTest',
	#superclass : 'TestCase',
	#category : 'Morphic-Tests-Layouts',
	#package : 'Morphic-Tests',
	#tag : 'Layouts'
}

{ #category : 'tests' }
TableLayoutTest >> testNotVisibleMorphIsNotGettingAnExtent [

	self 
		testNotVisibleMorphIsNotGettingAnExtent: #topToBottom 
		compareSelector: #height.
		
	self 
		testNotVisibleMorphIsNotGettingAnExtent: #leftToRight 
		compareSelector: #width
]

{ #category : 'tests' }
TableLayoutTest >> testNotVisibleMorphIsNotGettingAnExtent: direction compareSelector: aSelector [
	| morph m1 m2 |
	
	morph := Morph new
		layoutPolicy: TableLayout new;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		listDirection: direction;
		cellPositioning: #topLeft;
		listCentering: #topLeft;
		wrapCentering: #topLeft;
		cellInset: 0;
		borderWidth: 0;
		extent: 100@100;
		yourself.
	
	morph addMorphBack: (m1 := Morph new 
		color: Color red;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		yourself).
	morph addMorphBack: (m2 := Morph new 
		color: Color yellow;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		yourself).
	
	[  morph openInWorld.
	   self assert: (m1 perform: aSelector) equals: 50.
		self assert: (m2 perform: aSelector) equals: 50 ]
	ensure: [ morph delete ].
	
	m1 visible: false.
	[  morph openInWorld.
	   self assert: (m1 perform: aSelector) equals: 0.
		self assert: (m2 perform: aSelector) equals: 100 ]
	ensure: [ morph delete ].
	
	m1 visible: true.
	[  morph openInWorld.
	   self assert: (m1 perform: aSelector) equals: 50.
		self assert: (m2 perform: aSelector) equals: 50.
		m1 visible: false.
		self currentWorld doOneCycle.
	   self assert: (m1 perform: aSelector) equals: 0.
		self assert: (m2 perform: aSelector) equals: 100 ]
	ensure: [ morph delete ]
]
