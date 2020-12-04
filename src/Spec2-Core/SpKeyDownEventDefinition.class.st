"
I define a key down event
"
Class {
	#name : #SpKeyDownEventDefinition,
	#superclass : #SpBaseEventDefinition,
	#category : #'Spec2-Core-Base-Event'
}

{ #category : #installing }
SpKeyDownEventDefinition >> installOn: anAdapter target: aWidget [

	anAdapter installKeyDownEvent: self to: aWidget
]
