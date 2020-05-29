"
I define a mouse move event
"
Class {
	#name : #SpMouseMoveEventDefinition,
	#superclass : #SpBaseEventDefinition,
	#category : #'Spec2-Core-Base-Event'
}

{ #category : #installing }
SpMouseMoveEventDefinition >> installOn: anAdapter target: aWidget [

	anAdapter installMouseMoveEvent: self to: aWidget
]
