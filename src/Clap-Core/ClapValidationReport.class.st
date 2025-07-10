"
I aggregate multiple validation diagnostics together.
"
Class {
	#name : 'ClapValidationReport',
	#superclass : 'Object',
	#instVars : [
		'problems',
		'context'
	],
	#category : 'Clap-Core-Validation',
	#package : 'Clap-Core',
	#tag : 'Validation'
}

{ #category : 'instance creation' }
ClapValidationReport class >> on: aClapContext [

	^ self new
		  context: aClapContext;
		  yourself
]

{ #category : 'controlling' }
ClapValidationReport >> add: aDiagnostic [
	aDiagnostic isFailure ifTrue: [ problems add: aDiagnostic ].
]

{ #category : 'controlling' }
ClapValidationReport >> addAll: diagnostics [
	diagnostics do: [ :each | self add: each ]
]

{ #category : 'accessing' }
ClapValidationReport >> commandSpecification [
	^ context lastSubcommand specification
]

{ #category : 'initialization' }
ClapValidationReport >> context: aClapContext [

	context := aClapContext
]

{ #category : 'initialization' }
ClapValidationReport >> initialize [
	problems := OrderedCollection new
]

{ #category : 'testing' }
ClapValidationReport >> isFailure [
	^ self isSuccess not
]

{ #category : 'testing' }
ClapValidationReport >> isSuccess [
	^ problems isEmpty
]

{ #category : 'printing' }
ClapValidationReport >> printOn: aStream [
	problems do: [ :each |
		aStream
			nextPutAll: each printString;
			cr ]
]

{ #category : 'accessing' }
ClapValidationReport >> problems [
	
	^ problems
]
