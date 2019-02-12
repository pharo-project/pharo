"
I log when the middle mouse button has been clicked on me, which is useful for testing.
"
Class {
	#name : #MorphHandlingMiddleButton,
	#superclass : #Morph,
	#instVars : [
		'receivedBlueButtonUp',
		'receivedBlueButtonDown'
	],
	#category : #'SUnit-Support-UITesting'
}

{ #category : #'meta-actions' }
MorphHandlingMiddleButton >> blueButtonDown: anEvent [

	receivedBlueButtonDown := true.
]

{ #category : #'meta-actions' }
MorphHandlingMiddleButton >> blueButtonUp: anEvent [

	receivedBlueButtonUp := true.
]

{ #category : #'meta-actions' }
MorphHandlingMiddleButton >> handlerForBlueButtonDown: anEvent [

	^ self.
]

{ #category : #initialization }
MorphHandlingMiddleButton >> initialize [

	super initialize.
	receivedBlueButtonUp := receivedBlueButtonDown := false.
]

{ #category : #testing }
MorphHandlingMiddleButton >> wasClickedWithMiddleButton [

	^ receivedBlueButtonDown and: [ receivedBlueButtonUp ].
]
