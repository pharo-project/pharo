"
I check the image arguments and resolve if the access mode of the image. I am started very soon in the image start-up process.
"
Class {
	#name : #SessionAccessModeResolver,
	#superclass : #Object,
	#category : #'System-SessionManager-Utilities'
}

{ #category : #'class initialization' }
SessionAccessModeResolver class >> initialize [

	"self initialize"

	SessionManager default
		register: (ClassSessionHandler forClassNamed: self name)
		inCategory: SessionManager default systemCategory  
		atPriority: SessionManager default defaultPriority - 90.
]

{ #category : #'class initialization' }
SessionAccessModeResolver class >> resolve [ 

	"check if the image was started in the read-only mode"

	| arguments mode | 
	
	arguments := Smalltalk argumentsStartingAtIndex: 0.
			
	mode := #readWrite.

	(arguments anySatisfy: [ :each | each = '--readWriteAccessMode' ])
		ifTrue: [ mode := #readWrite ].
	(arguments anySatisfy: [ :each | each = '--readOnlyAccessMode' ])
		ifTrue: [ mode := #readOnly ].
	(arguments anySatisfy: [ :each | each = '--writeOnlyAccessMode' ])
		ifTrue: [ mode := #writeOnly ].
	(arguments anySatisfy: [ :each | each = '--disabledAccessMode' ])
		ifTrue: [ mode := #disabled ].
		
	SessionManager default currentSession accessMode: mode.
]

{ #category : #'class initialization' }
SessionAccessModeResolver class >> startUp: resuming [

	resuming ifTrue: [
		self resolve ]
]
