"
I am a dictionary holding UndeclaredVariables in the system. UndeclaredVariables represent the usage of a class that is not (yet) present in the system.

One goal is to transform me into a weak collection in the future so that we do not have to explicitly clean up the undeclareds and delegate the work to the GC.
"
Class {
	#name : 'UndeclaredRegistry',
	#superclass : 'IdentityDictionary',
	#category : 'System-Support-Utilities',
	#package : 'System-Support',
	#tag : 'Utilities'
}

{ #category : 'private' }
UndeclaredRegistry >> atNewIndex: index put: anUndeclaredVariable [

	anUndeclaredVariable isAssociation ifTrue: [
		self error: 'Only global variables should be added to the SystemDictionary and not associations. Undeclared: ' , anUndeclaredVariable name ].
	^ super atNewIndex: index put: anUndeclaredVariable
]
