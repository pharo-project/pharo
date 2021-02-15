"
I define a double click event
"
Class {
	#name : #SpMouseDoubleClickDefinition,
	#superclass : #SpBaseEventDefinition,
	#category : #'Spec2-Core-Base-Event'
}

{ #category : #installing }
SpMouseDoubleClickDefinition >> installOn: anAdapter target: aWidget [

	anAdapter installMouseDoubleClickEvent: self to: aWidget
]
