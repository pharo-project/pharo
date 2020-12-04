"
I represent the position of a popover in relation to its owner.
I am an abstract class and my children will define: top, left, bottom or right position.
"
Class {
	#name : #SpPopoverPosition,
	#superclass : #Object,
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #'Spec2-Core-Support'
}

{ #category : #accessing }
SpPopoverPosition class >> bottom [

	^ SpPopoverPositionBottom uniqueInstance
]

{ #category : #accessing }
SpPopoverPosition class >> left [

	^ SpPopoverPositionLeft uniqueInstance
]

{ #category : #'instance creation' }
SpPopoverPosition class >> new [

	self error: 'Use #uniqueInstance'
]

{ #category : #accessing }
SpPopoverPosition class >> right [

	^ SpPopoverPositionRight uniqueInstance
]

{ #category : #accessing }
SpPopoverPosition class >> top [

	^ SpPopoverPositionTop uniqueInstance
]

{ #category : #'instance creation' }
SpPopoverPosition class >> uniqueInstance [

	self = SpPopoverPosition 
		ifTrue: [ self error: 'I''m abstract, use one of my children.' ].	
	^ uniqueInstance ifNil: [ uniqueInstance := self basicNew initialize ]
]

{ #category : #operations }
SpPopoverPosition >> applyTo: aWidget [

	self subclassResponsibility
]
