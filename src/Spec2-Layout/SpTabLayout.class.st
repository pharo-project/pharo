Class {
	#name : #SpTabLayout,
	#superclass : #SpExecutableLayout,
	#instVars : [
		'initialPageIndex'
	],
	#category : #'Spec2-Layout-Tab'
}

{ #category : #private }
SpTabLayout >> adapterName [

	^ #TabAdapter
]

{ #category : #accessing }
SpTabLayout >> add: aName label: aString [

	self 
		add: aName 
		withConstraints: [ :constraints | 
			constraints label: aString ]
]

{ #category : #accessing }
SpTabLayout >> add: aPresenter withConstraints: aBlock [
	
	super add: aPresenter withConstraints: aBlock.
	self announceChildAdded: aPresenter
]

{ #category : #private }
SpTabLayout >> constraintsClass [

	^ SpTabConstraints
]

{ #category : #accessing }
SpTabLayout >> initialPageIndex [
	
	^ initialPageIndex
]

{ #category : #accessing }
SpTabLayout >> initialPageIndex: aNumber [

	initialPageIndex := aNumber
]

{ #category : #accessing }
SpTabLayout >> initialize [

	super initialize.
	initialPageIndex := 1
]
