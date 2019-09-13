Class {
	#name : #SpGridAlignment,
	#superclass : #Object,
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #'Spec2-Layout-Grid'
}

{ #category : #accessing }
SpGridAlignment class >> center [

	^ SpGridAlignmentCenter uniqueInstance
]

{ #category : #accessing }
SpGridAlignment class >> end [

	^ SpGridAlignmentEnd uniqueInstance
]

{ #category : #'instance creation' }
SpGridAlignment class >> new [ 

	self error: 'Use #uniqueInstance'
]

{ #category : #accessing }
SpGridAlignment class >> start [

	^ SpGridAlignmentStart uniqueInstance
]

{ #category : #'instance creation' }
SpGridAlignment class >> uniqueInstance [

	self = SpGridAlignment ifTrue: [ 
		self error: 'I''m an abstract class. Use one of my children instead.' ].
	^ uniqueInstance ifNil: [ uniqueInstance := self basicNew initialize ]
]
