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

{ #category : 'checking' }
UndeclaredRegistry >> verifyEmpty [
	"Check if the receiver is emtpy.
	If not, fail with an exception.
	The exception carries information about what are the offending entries and where they are"

	self ifEmpty: [ ^ self ].
	self error: 'Undeclared registry is not empty. Offending entries: '
		, (self associations collect: [ :a | a -> a usingMethods ]) printString
]
