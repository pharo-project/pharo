"
My instances represent formal parameters in command-line invocations.

My subclasses implement the various conventional kinds of command-line parameters, and their instances are composed to specify the syntax of a given command.
"
Class {
	#name : #ClapParameter,
	#superclass : #Object,
	#instVars : [
		'canonicalName',
		'description',
		'multiple',
		'meaningBlock',
		'validationBlock',
		'implicitMeaningBlock'
	],
	#category : #'Clap-Core-Specification'
}

{ #category : #evaluating }
ClapParameter class >> basicMeaning [
	^ [ :match | match ]
]

{ #category : #testing }
ClapParameter class >> isAbstract [
	^ self == ClapParameter
]

{ #category : #'instance creation' }
ClapParameter class >> withName: aString [ 
	^ self new
		canonicalName: aString;
		yourself
]

{ #category : #activating }
ClapParameter >> activateWith: arguments [
	"Convenience method for activating in an interactive context"
	^ (ClapContext specification: self)
		activateWith: arguments
]

{ #category : #adding }
ClapParameter >> addTo: parentParameter [
	^ self subclassResponsibility
]

{ #category : #accessing }
ClapParameter >> basicMeaning [
	^ self class basicMeaning
]

{ #category : #'matching - testing' }
ClapParameter >> canMatchAt: aStream [
	^ aStream atEnd not
		and: [ self canMatchWith: aStream peek ]
]

{ #category : #'matching - testing' }
ClapParameter >> canMatchWith: word [
	^ self subclassResponsibility
]

{ #category : #accessing }
ClapParameter >> canonicalName [
	^ canonicalName
]

{ #category : #initialization }
ClapParameter >> canonicalName: aString [ 
	canonicalName := aString
]

{ #category : #accessing }
ClapParameter >> description [
	^ description
]

{ #category : #initialization }
ClapParameter >> description: aString [
	description := aString
]

{ #category : #accessing }
ClapParameter >> implicitMeaning [
	^ implicitMeaningBlock
]

{ #category : #initialization }
ClapParameter >> implicitMeaning: aBlock [
	implicitMeaningBlock := aBlock
]

{ #category : #initialization }
ClapParameter >> initialize [
	description := ''.
	multiple := false.
	meaningBlock := self basicMeaning.
	implicitMeaningBlock := meaningBlock
]

{ #category : #testing }
ClapParameter >> isMultiple [
	^ multiple
]

{ #category : #matching }
ClapParameter >> match: aStreamOrSequenceable [
	^ self matchOn: aStreamOrSequenceable readStream
]

{ #category : #accessing }
ClapParameter >> matchClass [
	^ self subclassResponsibility
]

{ #category : #matching }
ClapParameter >> matchOn: aStream [
	^ (self newMatchAt: aStream)
		completeMatchOn: aStream;
		yourself
]

{ #category : #accessing }
ClapParameter >> meaning [
	^ meaningBlock
]

{ #category : #initialization }
ClapParameter >> meaning: aBlock [
	meaningBlock := aBlock
]

{ #category : #initialization }
ClapParameter >> multiple: aBoolean [
	multiple := aBoolean
]

{ #category : #matching }
ClapParameter >> newMatchAt: aStream [
	^ ((self canMatchAt: aStream)
				ifTrue: [ self matchClass ]
				ifFalse: [ ClapMismatch ])
			specification: self
]

{ #category : #printing }
ClapParameter >> printOn: aStream [
	super printOn: aStream.
	aStream
		nextPut: $(;
		nextPutAll: self canonicalName;
		nextPut: $)
]

{ #category : #documenting }
ClapParameter >> synopsis [
	^ String streamContents: [ :aStream | self synopsisOn: aStream ]
]

{ #category : #documenting }
ClapParameter >> synopsisOn: aStream [
	^ self subclassResponsibility
]

{ #category : #validation }
ClapParameter >> validate: aMatch on: aReport [
	aReport addAll: (self validationDiagnosticsFor: aMatch)
]

{ #category : #validation }
ClapParameter >> validationDiagnosticsFor: aMatch [
	^ validationBlock
		ifNil: [ #() ]
		ifNotNil: [ validationBlock value: aMatch ]
]

{ #category : #initialization }
ClapParameter >> validations: aBlock [
	validationBlock := aBlock
]

{ #category : #accessing }
ClapParameter >> valueFor: aMatch with: arg [
	^ self meaning
		cull: aMatch
		cull: arg
]

{ #category : #accessing }
ClapParameter >> valueForImplicit: aMatch with: arg [
	^ self implicitMeaning cull: aMatch cull: arg
]
