"
A SpecLayoutSend is a spec layout action used when a selector is sent to the layout
"
Class {
	#name : #SpLayoutSend,
	#superclass : #SpAbstractSpecLayoutAction,
	#category : #'Spec2-Layout-Deprecated'
}

{ #category : #'instance creation' }
SpLayoutSend class >> selector: selector [

	^ self new
		selector: selector;
		yourself
]

{ #category : #accesing }
SpLayoutSend >> arguments [
	^ {  }
]

{ #category : #protocol }
SpLayoutSend >> bottomFraction [

	^ 0
]

{ #category : #protocol }
SpLayoutSend >> bottomFraction: aNumber [

	^ 0
]

{ #category : #protocol }
SpLayoutSend >> bottomOffset [

	^ 0
]

{ #category : #protocol }
SpLayoutSend >> bottomOffset: aNumber [

	^ 0
]

{ #category : #testing }
SpLayoutSend >> isSplitter [

	^ false
]

{ #category : #protocol }
SpLayoutSend >> leftFraction [

	^ 0
]

{ #category : #protocol }
SpLayoutSend >> leftFraction: aNumber [

	^ 0
]

{ #category : #protocol }
SpLayoutSend >> leftOffset [

	^ 0
]

{ #category : #protocol }
SpLayoutSend >> leftOffset: aNumber [

	^ 0
]

{ #category : #protocol }
SpLayoutSend >> rightFraction [

	^ 0
]

{ #category : #protocol }
SpLayoutSend >> rightFraction: aNumber [

	^ 0
]

{ #category : #protocol }
SpLayoutSend >> rightOffset [

	^ 0
]

{ #category : #protocol }
SpLayoutSend >> rightOffset: aNumber [

	^ 0
]

{ #category : #protocol }
SpLayoutSend >> topFraction [

	^ 0
]

{ #category : #protocol }
SpLayoutSend >> topFraction: aNumber [

	^ 0
]

{ #category : #protocol }
SpLayoutSend >> topOffset [

	^ 0
]

{ #category : #protocol }
SpLayoutSend >> topOffset: aNumber [

	^ 0
]
