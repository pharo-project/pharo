Class {
	#name : #SpecLayoutDirection,
	#superclass : #Object,
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #'Spec-Layout-Base'
}

{ #category : #accessing }
SpecLayoutDirection class >> horizontal [

	^ SpecLayoutDirectionHorizontal uniqueInstance
]

{ #category : #accessing }
SpecLayoutDirection class >> uniqueInstance [ 
	
	self = SpecLayoutDirection ifTrue: [ 
		self error: 'I''m abstract, use one of my children' ].
	^ uniqueInstance ifNil: [ uniqueInstance := self basicNew initialize ]
]

{ #category : #accessing }
SpecLayoutDirection class >> vertical [

	^ SpecLayoutDirectionVertical uniqueInstance
]

{ #category : #factory }
SpecLayoutDirection >> newWidgetOn: anAdapter [

	^ self subclassResponsibility
]
