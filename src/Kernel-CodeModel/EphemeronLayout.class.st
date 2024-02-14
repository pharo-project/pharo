"
A layout for ephemeron objects
"
Class {
	#name : 'EphemeronLayout',
	#superclass : 'PointerLayout',
	#category : 'Kernel-CodeModel-Layout',
	#package : 'Kernel-CodeModel',
	#tag : 'Layout'
}

{ #category : 'instance creation' }
EphemeronLayout class >> extending: superLayout scope: aScope host: aClass [

	^ (superLayout extendEphemeron: aScope)
		host: aClass;
		yourself
]

{ #category : 'format' }
EphemeronLayout >> instanceSpecification [
	^ 5
]
