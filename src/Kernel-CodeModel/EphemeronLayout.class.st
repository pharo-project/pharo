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

{ #category : 'description' }
EphemeronLayout class >> subclassDefiningSymbol [
	"Answer a keyword that describes the receiver's kind of subclass
	Note: this is for printing the ST80 style class definiton, usage makes no sense for anything
	else!"
	^#ephemeronSubclass:
]

{ #category : 'format' }
EphemeronLayout >> instanceSpecification [
	^ 5
]
