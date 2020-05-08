"
I define a key up event
"
Class {
	#name : #SpKeyUpEventDefinition,
	#superclass : #SpBaseEventDefinition,
	#category : #'Spec2-Core-Base-Event'
}

{ #category : #installing }
SpKeyUpEventDefinition >> installOn: anAdapter target: aWidget [

	anAdapter installKeyUpEvent: self to: aWidget
]
