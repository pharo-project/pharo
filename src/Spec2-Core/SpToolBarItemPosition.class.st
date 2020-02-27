"
Abstract superclass for item positioning. Each subclass should only have one instance.

The default item positioning is left.

  	ITItemPosition default
	ITItemPosition right
	ITItemPosition left
"
Class {
	#name : #SpToolBarItemPosition,
	#superclass : #Object,
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #'Spec2-Core-Widgets'
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
