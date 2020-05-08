"
I define a mouse down event
"
Class {
	#name : #SpMouseDownEventDefinition,
	#superclass : #SpBaseEventDefinition,
	#category : #'Spec2-Core-Base-Event'
}

{ #category : #installing }
SpMouseDownEventDefinition >> installOn: anAdapter target: aWidget [

	anAdapter installMouseDownEvent: self to: aWidget
]
