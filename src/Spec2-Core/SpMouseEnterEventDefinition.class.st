"
I define a mouse enter event
"
Class {
	#name : #SpMouseEnterEventDefinition,
	#superclass : #SpBaseEventDefinition,
	#category : #'Spec2-Core-Base-Event'
}

{ #category : #installing }
SpMouseEnterEventDefinition >> installOn: anAdapter target: aWidget [

	anAdapter installMouseEnterEvent: self to: aWidget
]
