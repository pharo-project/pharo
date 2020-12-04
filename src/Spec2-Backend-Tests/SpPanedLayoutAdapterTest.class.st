Class {
	#name : #SpPanedLayoutAdapterTest,
	#superclass : #SpLayoutAdapterTest,
	#category : #'Spec2-Backend-Tests-Layout'
}

{ #category : #testing }
SpPanedLayoutAdapterTest class >> isAbstract [

	^ self == SpPanedLayoutAdapterTest
]

{ #category : #tests }
SpPanedLayoutAdapterTest >> testAdapterElementsAreInExplicitOrder [

	| first second |
	layout addFirst: (first := SpButtonPresenter new).
	layout addSecond: (second := SpButtonPresenter new).
	self assert: self adapter children first equals: first adapter widget.
	self assert: self adapter children second equals: second adapter widget.
]

{ #category : #tests }
SpPanedLayoutAdapterTest >> testAddFirstElementAfterOpenAddsToAdapter [

	layout addSecond: SpButtonPresenter new.
	self openInstance.
	
	layout addFirst: SpButtonPresenter new.
	self assert: self adapter children size equals: 2
]

{ #category : #tests }
SpPanedLayoutAdapterTest >> testAddFirstTwiceReplacesChild [

	| first second |
	layout addFirst: (first := SpButtonPresenter new).
	layout addFirst: (second := SpButtonPresenter new).
	self assert: self adapter children first equals: second adapter widget
]

{ #category : #tests }
SpPanedLayoutAdapterTest >> testAddFirstTwiceSetsChildrenSingleTime [

	| first second |
	layout addFirst: (first := SpButtonPresenter new).
	layout addFirst: (second := SpButtonPresenter new).
	self assert: self adapter children size equals: 1
]

{ #category : #tests }
SpPanedLayoutAdapterTest >> testAddSecondElementAfterOpenAddsToAdapter [

	layout addFirst: SpButtonPresenter new.
	self openInstance.
	
	layout addSecond: SpButtonPresenter new.
	self assert: self adapter children size equals: 2
]

{ #category : #tests }
SpPanedLayoutAdapterTest >> testAddSecondTwiceReplacesChild [

	| first second |
	layout addSecond: (first := SpButtonPresenter new).
	layout addSecond: (second := SpButtonPresenter new).
	self assert: self adapter children first equals: second adapter widget
]

{ #category : #tests }
SpPanedLayoutAdapterTest >> testAddSecondTwiceSetsChildrenSingleTime [

	| first second |
	layout addSecond: (first := SpButtonPresenter new).
	layout addSecond: (second := SpButtonPresenter new).
	self assert: self adapter children size equals: 1
]

{ #category : #tests }
SpPanedLayoutAdapterTest >> testLayoutWithFirstElementIsNotEmpty [

	layout addFirst: SpButtonPresenter new.
	self deny: self adapter isEmpty
]

{ #category : #tests }
SpPanedLayoutAdapterTest >> testLayoutWithSecondElementIsNotEmpty [

	layout addSecond: SpButtonPresenter new.
	self deny: self adapter isEmpty
]

{ #category : #tests }
SpPanedLayoutAdapterTest >> testRemoveFirstElementAddedAfterOpenRemovesFromAdapter [

	| element |
	layout addFirst: (element := SpButtonPresenter new).
	self openInstance.
	
	layout remove: element.
	self assert: self adapter isEmpty
]

{ #category : #tests }
SpPanedLayoutAdapterTest >> testRemoveFirstElementRemovesFromAdapter [

	| element |
	layout addFirst: (element := SpButtonPresenter new).
	layout remove: element.
	self assert: self adapter isEmpty
]

{ #category : #tests }
SpPanedLayoutAdapterTest >> testRemoveSecondElementAddedAfterOpenRemovesFromAdapter [

	| element |
	layout addSecond: (element := SpButtonPresenter new).
	self openInstance.
	
	layout remove: element.
	self assert: self adapter isEmpty
]

{ #category : #tests }
SpPanedLayoutAdapterTest >> testRemoveSecondElementRemovesFromAdapter [

	| element |
	layout addSecond: (element := SpButtonPresenter new).
	layout remove: element.
	self assert: self adapter isEmpty
]
