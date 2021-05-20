Class {
	#name : #SpGridLayoutAdapterTest,
	#superclass : #SpLayoutAdapterTest,
	#category : #'Spec2-Backend-Tests-Layout'
}

{ #category : #accessing }
SpGridLayoutAdapterTest >> newLayout [

	^ SpGridLayout new
]

{ #category : #tests }
SpGridLayoutAdapterTest >> testAddAtSamePositionTwiceReplacesChild [

	| first second |
	layout add: (first := SpButtonPresenter new) at: 1@1.
	layout add: (second := SpButtonPresenter new) at: 1@1.
	self assert: self adapter children first equals: second adapter widget
]

{ #category : #tests }
SpGridLayoutAdapterTest >> testAddAtSamePositionTwiceSetsChildrenSingleTime [

	layout add: (SpButtonPresenter new) at: 1@1.
	layout add: (SpButtonPresenter new) at: 1@1.
	self assert: self adapter children size equals: 1
]

{ #category : #tests }
SpGridLayoutAdapterTest >> testAddElementAddsToAdapter [

	layout add: SpButtonPresenter new at: 1@1.
	self deny: self adapter isEmpty
]

{ #category : #tests }
SpGridLayoutAdapterTest >> testAddElementAfterOpenAddsToAdapter [

	layout add: SpButtonPresenter new at: 1@1.
	self openInstance.
	
	layout add: SpButtonPresenter new at: 1@2.
	self assert: self adapter children size equals: 2
]

{ #category : #tests }
SpGridLayoutAdapterTest >> testBeColumnHomogeneous [

	layout 
		beColumnHomogeneous;
		add: 'Test' at: 1@1; 
		add: 'Other label test' at: 2@1.
		
	self openInstance.
	
	self 
		assert: presenter adapter widget submorphs first width 
		equals: presenter adapter widget submorphs second width
]

{ #category : #tests }
SpGridLayoutAdapterTest >> testBeColumnNotHomogeneous [

	layout 
		beColumnNotHomogeneous;
		add: 'Test' at: 1@1; 
		add: 'Other label test' at: 2@1.
		
	self openInstance.
	
	self 
		deny: presenter adapter widget submorphs first width 
		equals: presenter adapter widget submorphs second width
]

{ #category : #tests }
SpGridLayoutAdapterTest >> testBeRowHomogeneous [

	layout 
		beRowHomogeneous;
		add: 'Test' at: 1@1; 
		add: (presenter newMorph morph: (Morph new extent: 40@40)) at: 1@2.
		
	self openInstance.
	
	self 
		assert: presenter adapter widget submorphs first height 
		equals: presenter adapter widget submorphs second height
]

{ #category : #tests }
SpGridLayoutAdapterTest >> testBeRowNotHomogeneous [
	| testMorph |
	
	testMorph := Morph new
		vResizing: #rigid;
		extent: 40@40;
		yourself.
	
	layout 
		beRowNotHomogeneous;
		add: 'Test' at: 1@1; 
		add: (presenter newMorph morph: testMorph) at: 1@2.
		
	self openInstance.
	
	self 
		deny: presenter adapter widget submorphs first height 
		equals: presenter adapter widget submorphs second height
]

{ #category : #tests }
SpGridLayoutAdapterTest >> testColumnSpanColumnHomogeneous [

	layout 
		beColumnHomogeneous;
		add: 'Test' at: 1@1 span: 2@1; 
		add: 'Other label test' at: 1@2; add: 'span column'at: 2@2.
		
	self openInstance.
	
	self 
		assert: presenter adapter widget submorphs first width 
		equals: (
			(presenter adapter widget submorphs second width) 
			+ (presenter adapter widget submorphs third width))
]

{ #category : #tests }
SpGridLayoutAdapterTest >> testColumnSpanColumnNotHomogeneous [

	layout 
		beColumnNotHomogeneous;
		add: 'Test' at: 1@1 span: 2@1; 
		add: 'Other' at: 1@2; add: 'span column, larger'at: 2@2.
		
	self openInstance.
	
	"the first column width is equal to the sum of two columns"
	self 
		assert: presenter adapter widget submorphs first width 
		equals: (
			(presenter adapter widget submorphs second width) 
			+ (presenter adapter widget submorphs third width)).
	"the first column still needs to be smaller than the first"
	self 
		assert: presenter adapter widget submorphs second width < (presenter adapter widget submorphs third width)
]

{ #category : #tests }
SpGridLayoutAdapterTest >> testRemoveElementAddedAfterOpenRemovesFromAdapter [

	| element |
	layout add: (element := SpButtonPresenter new) at: 1@1.
	self openInstance.
	
	layout remove: element.
	self assert: self adapter isEmpty
]

{ #category : #tests }
SpGridLayoutAdapterTest >> testRemoveElementRemovesFromAdapter [

	| element |
	layout add: (element := SpButtonPresenter new) at: 1@1.
	layout remove: element.
	self assert: self adapter isEmpty
]

{ #category : #tests }
SpGridLayoutAdapterTest >> testRowSpanRowHomogeneous [
	| testMorph |
	
	testMorph := Morph new
		vResizing: #rigid;
		extent: 40@40;
		yourself.
	
	layout 
		beRowHomogeneous;
		add: 'Test 1' at: 1@1; add: (presenter newMorph morph: testMorph) at: 2@1 span: 1@2;
		add: 'Test 2' at: 1@2.
		
	self openInstance.
	self 
		assert: presenter adapter widget submorphs second height 
		equals: 
			presenter adapter widget submorphs first height 
			+ presenter adapter widget submorphs third height
]

{ #category : #tests }
SpGridLayoutAdapterTest >> testRowSpanRowNotHomogeneous [
	| testMorph |
	
	testMorph := Morph new
		vResizing: #rigid;
		extent: 40@40;
		yourself.
	
	layout 
		beRowNotHomogeneous;
		add: 'Test 1' at: 1@1; add: (presenter newMorph morph: testMorph) at: 2@1 span: 1@2;
		add: 'Test 2' at: 1@2.
		
	self openInstance.
	
	self 
		assert: presenter adapter widget submorphs second height 
		equals: 
			presenter adapter widget submorphs first height 
			+ presenter adapter widget submorphs third height
]
