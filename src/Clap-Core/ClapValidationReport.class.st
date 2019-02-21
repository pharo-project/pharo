"
I aggregate multiple validation diagnostics together.
"
Class {
	#name : #ClapValidationReport,
	#superclass : #Object,
	#instVars : [
		'problems'
	],
	#category : #'Clap-Core-Validation'
}

{ #category : #'instance creation' }
ClapValidationReport class >> success [
	^ self new
]

{ #category : #controlling }
ClapValidationReport >> add: aDiagnostic [
	aDiagnostic isFailure ifTrue: [ problems add: aDiagnostic ].

]

{ #category : #controlling }
ClapValidationReport >> addAll: diagnostics [
	diagnostics do: [ :each | self add: each ]
]

{ #category : #initialization }
ClapValidationReport >> initialize [
	problems := OrderedCollection new
]

{ #category : #testing }
ClapValidationReport >> isFailure [
	^ self isSuccess not
]

{ #category : #testing }
ClapValidationReport >> isSuccess [
	^ problems isEmpty
]

{ #category : #printing }
ClapValidationReport >> printOn: aStream [
	problems do: [ :each |
		aStream
			nextPutAll: each printString;
			cr ]
]
