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
