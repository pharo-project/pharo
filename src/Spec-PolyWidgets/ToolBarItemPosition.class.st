"
Abstract superclass for item positioning. Each subclass should only have one instance.

The default item positioning is left.

  	ITItemPosition default
	ITItemPosition right
	ITItemPosition left
"
Class {
	#name : #ToolBarItemPosition,
	#superclass : #Object,
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #'Spec-PolyWidgets-ToolBar'
}

{ #category : #accessing }
ToolBarItemPosition class >> default [
	^ self left	
]

{ #category : #accessing }
ToolBarItemPosition class >> left [
	^ ToolBarItemPositionLeft uniqueInstance
	
]

{ #category : #'instance creation' }
ToolBarItemPosition class >> new [
	self error: 'Use uniqueInstance'
]

{ #category : #accessing }
ToolBarItemPosition class >> right [
	^ ToolBarItemPositionRight uniqueInstance
	
]

{ #category : #'instance creation' }
ToolBarItemPosition class >> uniqueInstance [

	^ uniqueInstance ifNil: [ uniqueInstance := self basicNew initialize ]
]

{ #category : #accessing }
ToolBarItemPosition >> addItem: anObject into: aToolbar [
	self subclassResponsibility
]
