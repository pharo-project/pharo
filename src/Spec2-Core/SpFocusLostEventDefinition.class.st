"
I define a ""lost focus"" (focus leaved the widget) event.

"
Class {
	#name : #SpFocusLostEventDefinition,
	#superclass : #SpBaseEventDefinition,
	#category : #'Spec2-Core-Base-Event'
}

{ #category : #installing }
SpFocusLostEventDefinition >> installOn: anAdapter target: aWidget [

	anAdapter installFocusLostEvent: self to: aWidget
]
