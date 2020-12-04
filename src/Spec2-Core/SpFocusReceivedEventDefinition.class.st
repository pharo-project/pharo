"
I define a ""got focus"" (focus received) event.

"
Class {
	#name : #SpFocusReceivedEventDefinition,
	#superclass : #SpBaseEventDefinition,
	#category : #'Spec2-Core-Base-Event'
}

{ #category : #installing }
SpFocusReceivedEventDefinition >> installOn: anAdapter target: aWidget [

	anAdapter installFocusReceivedEvent: self to: aWidget
]
