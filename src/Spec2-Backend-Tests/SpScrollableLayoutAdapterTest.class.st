Class {
	#name : #SpScrollableLayoutAdapterTest,
	#superclass : #SpLayoutAdapterTest,
	#category : #'Spec2-Backend-Tests-Layout'
}

{ #category : #tests }
SpScrollableLayoutAdapterTest >> newLayout [

	^ SpScrollableLayout new
]

{ #category : #tests }
SpScrollableLayoutAdapterTest >> testAddTwiceReplacesChild [

	| first second |
	layout child: (first := SpButtonPresenter new).
	layout child: (second := SpButtonPresenter new).
	self assert: self adapter children first equals: second adapter widget
]

{ #category : #tests }
SpScrollableLayoutAdapterTest >> testAddTwiceSetsChildrenSingleTime [

	| first second |
	layout child: (first := SpButtonPresenter new).
	layout child: (second := SpButtonPresenter new).
	self assert: self adapter children size equals: 1
]

{ #category : #tests }
SpScrollableLayoutAdapterTest >> testLayoutWithChildIsNotEmpty [

	layout child: SpButtonPresenter new.
	self deny: self adapter isEmpty
]

{ #category : #tests }
SpScrollableLayoutAdapterTest >> testRemoveChildAddedAfterOpenRemovesFromAdapter [

	| element |
	layout child: (element := SpButtonPresenter new).
	self openInstance.
	
	layout remove: element.
	self assert: self adapter isEmpty
]

{ #category : #tests }
SpScrollableLayoutAdapterTest >> testRemoveChildElementRemovesFromAdapter [

	| element |
	layout child: (element := SpButtonPresenter new).
	layout remove: element.
	self assert: self adapter isEmpty
]
