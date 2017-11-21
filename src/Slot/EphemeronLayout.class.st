"
A layout for ephemeron objects
"
Class {
	#name : #EphemeronLayout,
	#superclass : #PointerLayout,
	#category : #Slot-Layout
}

{ #category : #'as yet unclassified' }
EphemeronLayout class >> extending: superLayout scope: aScope host: aClass [ 
	
	^ (superLayout extendEphemeron: aScope)
		host: aClass;
		yourself
]

{ #category : #format }
EphemeronLayout >> instanceSpecification [
	^ 5
]
