"
I'm a class responsible for item positioning. My subclass have only have one instance responsible for the different behavior depending on direction. My users should not refer to my subclasses but to myself e.g, 

SpToolBarItemPosition left or SpToolBarItemPosition right
"
Class {
	#name : #SpToolBarItemPosition,
	#superclass : #Object,
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #'Spec2-PolyWidgets-ToolBar'
}

{ #category : #accessing }
SpToolBarItemPosition class >> default [
	^ self left	
]

{ #category : #accessing }
SpToolBarItemPosition class >> left [
	^ SpToolBarItemPositionLeft uniqueInstance
	
]

{ #category : #'instance creation' }
SpToolBarItemPosition class >> new [
	self error: 'Use uniqueInstance'
]

{ #category : #accessing }
SpToolBarItemPosition class >> right [
	^ SpToolBarItemPositionRight uniqueInstance
	
]

{ #category : #'instance creation' }
SpToolBarItemPosition class >> uniqueInstance [

	^ uniqueInstance ifNil: [ uniqueInstance := self basicNew initialize ]
]

{ #category : #accessing }
SpToolBarItemPosition >> addItem: anObject into: aToolbar [
	self subclassResponsibility
]
