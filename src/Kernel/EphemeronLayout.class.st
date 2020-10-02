"
A layout for ephemeron objects
"
Class {
	#name : #EphemeronLayout,
	#superclass : #PointerLayout,
	#category : #'Kernel-Layout'
}

{ #category : #format }
EphemeronLayout >> instanceSpecification [
	^ 5
]

{ #category : #accessing }
EphemeronLayout >> kindOfSubclass [
	^' ephemeronSubclass: '
]

{ #category : #accessing }
EphemeronLayout >> typeOfClass [
	^#ephemeron
]
