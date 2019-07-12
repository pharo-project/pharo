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

	(Smalltalk argumentsInclude: '--readWriteAccessMode')
		ifTrue: [ mode := #readWrite ].
	(Smalltalk argumentsInclude:  '--readOnlyAccessMode')
		ifTrue: [ mode := #readOnly ].
	(Smalltalk argumentsInclude: '--writeOnlyAccessMode')
		ifTrue: [ mode := #writeOnly ].
	(Smalltalk argumentsInclude: '--disabledAccessMode')
		ifTrue: [ mode := #disabled ].
		
	SessionManager default currentSession accessMode: mode.
]

{ #category : #'class initialization' }
SessionAccessModeResolver class >> startUp: resuming [

	resuming ifTrue: [
		self resolve ]
]
