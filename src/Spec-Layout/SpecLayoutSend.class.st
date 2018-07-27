"
A SpecLayoutSend is a spec layout action used when a selector is sent to the layout
"
Class {
	#name : #SpecLayoutSend,
	#superclass : #AbstractSpecLayoutAction,
	#category : #'Spec-Layout-Actions'
}

{ #category : #'instance creation' }
SpecLayoutSend class >> selector: selector arguments: arguments [

	^ self new
		arguments: arguments;
		selector: selector;
		yourself
]

{ #category : #protocol }
SpecLayoutSend >> asSpecElements [

	^ {self selector.}, self arguments
]

{ #category : #protocol }
SpecLayoutSend >> bottomFraction [

	^ 0
]

{ #category : #protocol }
SpecLayoutSend >> bottomFraction: aNumber [

	^ 0
]

{ #category : #protocol }
SpecLayoutSend >> bottomOffset [

	^ 0
]

{ #category : #protocol }
SpecLayoutSend >> bottomOffset: aNumber [

	^ 0
]

{ #category : #testing }
SpecLayoutSend >> isSplitter [

	^ false
]

{ #category : #protocol }
SpecLayoutSend >> leftFraction [

	^ 0
]

{ #category : #protocol }
SpecLayoutSend >> leftFraction: aNumber [

	^ 0
]

{ #category : #protocol }
SpecLayoutSend >> leftOffset [

	^ 0
]

{ #category : #protocol }
SpecLayoutSend >> leftOffset: aNumber [

	^ 0
]

{ #category : #protocol }
SpecLayoutSend >> rightFraction [

	^ 0
]

{ #category : #protocol }
SpecLayoutSend >> rightFraction: aNumber [

	^ 0
]

{ #category : #protocol }
SpecLayoutSend >> rightOffset [

	^ 0
]

{ #category : #protocol }
SpecLayoutSend >> rightOffset: aNumber [

	^ 0
]

{ #category : #protocol }
SpecLayoutSend >> topFraction [

	^ 0
]

{ #category : #protocol }
SpecLayoutSend >> topFraction: aNumber [

	^ 0
]

{ #category : #protocol }
SpecLayoutSend >> topOffset [

	^ 0
]

{ #category : #protocol }
SpecLayoutSend >> topOffset: aNumber [

	^ 0
]
