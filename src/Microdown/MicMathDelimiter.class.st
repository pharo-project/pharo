"
I'm a delimiter for math inline. An example is $V_i = C_0 - C_3$.
I delimits `MicMathInlineBlock`.
"
Class {
	#name : #MicMathDelimiter,
	#superclass : #MicAbstractDelimiter,
	#category : #'Microdown-ModelInline'
}

{ #category : #accessing }
MicMathDelimiter class >> associatedInlineBlock [
	^ MicMathInlineBlock 
]

{ #category : #accessing }
MicMathDelimiter class >> isCloser [
	^ true
]

{ #category : #accessing }
MicMathDelimiter class >> isOpener [
	^ true
]

{ #category : #accessing }
MicMathDelimiter class >> markup [
	^ '$'
]

{ #category : #accessing }
MicMathDelimiter class >> type [
	^ #math
]
