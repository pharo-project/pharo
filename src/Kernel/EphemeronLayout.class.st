"
A layout for ephemeron objects
"
Class {
	#name : #EphemeronLayout,
	#superclass : #PointerLayout,
	#category : #'Kernel-Layout'
}

{ #category : #'instance creation' }
EphemeronLayout class >> extending: superLayout scope: aScope host: aClass [ 
	
	^ (superLayout extendEphemeron: aScope)
		host: aClass;
		yourself
]

{ #category : #description }
EphemeronLayout class >> mcTypeSymbol [ 
	^ #ephemeron
]

{ #category : #description }
EphemeronLayout class >> subclassDefiningSymbol [
	^#ephemeronSubclass:
]

{ #category : #format }
EphemeronLayout >> instanceSpecification [
	^ 5
]
