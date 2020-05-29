"
I indicate the alignment of the presenter in relation with the layout.
"
Class {
	#name : #SpLayoutWidgetAlignment,
	#superclass : #Object,
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #'Spec2-Layout-Base'
}

{ #category : #accessing }
SpLayoutWidgetAlignment class >> center [

	^ SpLayoutWidgetAlignmentCenter uniqueInstance
]

{ #category : #accessing }
SpLayoutWidgetAlignment class >> end [

	^ SpLayoutWidgetAlignmentEnd uniqueInstance
]

{ #category : #'instance creation' }
SpLayoutWidgetAlignment class >> new [ 

	self error: 'Use #uniqueInstance'
]

{ #category : #accessing }
SpLayoutWidgetAlignment class >> start [

	^ SpLayoutWidgetAlignmentStart uniqueInstance
]

{ #category : #'instance creation' }
SpLayoutWidgetAlignment class >> uniqueInstance [

	self = SpLayoutWidgetAlignment ifTrue: [ 
		self error: 'I''m an abstract class. Use one of my children instead.' ].
	^ uniqueInstance ifNil: [ uniqueInstance := self basicNew initialize ]
]
