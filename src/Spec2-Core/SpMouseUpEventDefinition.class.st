"
I define a mouse up event
"
Class {
	#name : #SpMouseUpEventDefinition,
	#superclass : #SpBaseEventDefinition,
	#category : #'Spec2-Core-Base-Event'
}

{ #category : #installing }
SpMouseUpEventDefinition >> installOn: anAdapter target: aWidget [

	anAdapter installMouseUpEvent: self to: aWidget
]
