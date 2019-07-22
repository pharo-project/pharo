Class {
	#name : #SpLayoutDirection,
	#superclass : #Object,
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #'Spec2-Layout-Base'
}

{ #category : #accessing }
SpLayoutDirection class >> horizontal [

	^ SpLayoutDirectionHorizontal uniqueInstance
]

{ #category : #accessing }
SpLayoutDirection class >> uniqueInstance [ 
	
	self = SpLayoutDirection ifTrue: [ 
		self error: 'I''m abstract, use one of my children' ].
	^ uniqueInstance ifNil: [ uniqueInstance := self basicNew initialize ]
]

{ #category : #accessing }
SpLayoutDirection class >> vertical [

	^ SpLayoutDirectionVertical uniqueInstance
]

{ #category : #factory }
SpLayoutDirection >> newWidgetOn: anAdapter [

	^ self subclassResponsibility
]
