"
I am a dictionary holding UndeclaredVariables in the system. UndeclaredVariables represent the usage of a class that is not (yet) present in the system.

I am named UndeclaredHolder instead of UndeclaredDictionary because one goal is to transform me into a weak collection in the future so that we do not have to explicitly clean up the undeclareds and delegate the work to the GC.
"
Class {
	#name : 'UndeclaredHolder',
	#superclass : 'IdentityDictionary',
	#category : 'System-Support-Utilities',
	#package : 'System-Support',
	#tag : 'Utilities'
}

{ #category : 'private' }
UndeclaredHolder >> atNewIndex: index put: aUndeclaredVariable [

	aUndeclaredVariable isAssociation ifTrue: [ self error: 'Only undeclared variables should be added to the undeclareds and not associations.' ].
	^ super atNewIndex: index put: aUndeclaredVariable
]
