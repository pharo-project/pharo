"
My instances represent formal parameters in command-line invocations.

My subclasses implement the various conventional kinds of command-line parameters, and their instances are composed to specify the syntax of a given command.
"
Class {
	#name : 'ClapParameter',
	#superclass : 'Object',
	#instVars : [
		'identifier',
		'canonicalName',
		'description',
		'multiple',
		'meaningBlock',
		'validationBlock',
		'implicitMeaningBlock'
	],
	#category : 'Clap-Core-Specification',
	#package : 'Clap-Core',
	#tag : 'Specification'
}

{ #category : 'evaluating' }
ClapParameter class >> basicMeaning [
	^ [ :match | match ]
]

{ #category : 'instance creation' }
ClapParameter class >> id: anIdentifier [
	^ self new
		identifier: anIdentifier asSymbol;
		yourself
]

{ #category : 'testing' }
ClapParameter class >> isAbstract [
	^ self == ClapParameter
]

{ #category : 'activating' }
ClapParameter >> activateWith: arguments [
	"Convenience method for activating in an interactive context"
	^ (self activationWith: arguments) execute
]

{ #category : 'activating' }
ClapParameter >> activation [
	^ ClapContext specification: self
]

{ #category : 'activating' }
ClapParameter >> activationWith: arguments [
	^ self activation
		arguments: arguments;
		yourself
]

{ #category : 'adding' }
ClapParameter >> addTo: parentParameter [
	^ self subclassResponsibility
]

{ #category : 'converting' }
ClapParameter >> asClapIdentifier [
	^ self identifier
]

{ #category : 'accessing' }
ClapParameter >> basicMeaning [
	^ self class basicMeaning
]

{ #category : 'matching - testing' }
ClapParameter >> canMatchAt: aStream [
	^ aStream atEnd not
		and: [ self canMatchWith: aStream peek ]
]

{ #category : 'matching - testing' }
ClapParameter >> canMatchWith: word [
	^ self subclassResponsibility
]

{ #category : 'accessing' }
ClapParameter >> canonicalName [
	^ canonicalName ifNil: [ self identifier asString ]
]

{ #category : 'initialization' }
ClapParameter >> canonicalName: aName [
	canonicalName := aName asString
]

{ #category : 'accessing' }
ClapParameter >> description [
	^ description
]

{ #category : 'initialization' }
ClapParameter >> description: aString [
	description := aString
]

{ #category : 'accessing' }
ClapParameter >> identifier [
	^ identifier
]

{ #category : 'initialization' }
ClapParameter >> identifier: anIdentifier [
	identifier := anIdentifier
]

{ #category : 'testing' }
ClapParameter >> identifiesClapParameter: aParameter [
	^ self == aParameter
]

{ #category : 'accessing' }
ClapParameter >> implicitMeaning [
	^ implicitMeaningBlock
]

{ #category : 'initialization' }
ClapParameter >> implicitMeaning: aBlock [
	implicitMeaningBlock := aBlock
]

{ #category : 'initialization' }
ClapParameter >> initialize [
	description := ''.
	multiple := false.
]

{ #category : 'testing' }
ClapParameter >> isCommand [

	^ false
]

{ #category : 'testing' }
ClapParameter >> isFlag [ 

	^ false
]

{ #category : 'testing' }
ClapParameter >> isMatchableIn: aCompositeMatch [
	^ self isMultiple or: [ (aCompositeMatch includesMatchOf: self) not ]
]

{ #category : 'testing' }
ClapParameter >> isMultiple [
	^ multiple
]

{ #category : 'testing' }
ClapParameter >> isPositional [ 

	^ false
]

{ #category : 'matching' }
ClapParameter >> match: aSequenceable [
	"Convenience for tests; this creates an adhoc context."
	^ (self activationWith: aSequenceable) match
]

{ #category : 'accessing' }
ClapParameter >> matchClass [
	^ self subclassResponsibility
]

{ #category : 'matching' }
ClapParameter >> matchOn: aStream in: aContext [
	^ (self newMatchAt: aStream in: aContext)
		completeMatchOn: aStream;
		yourself
]

{ #category : 'accessing' }
ClapParameter >> meaning [
	^ meaningBlock
]

{ #category : 'initialization' }
ClapParameter >> meaning: aBlock [
	meaningBlock := aBlock
]

{ #category : 'initialization' }
ClapParameter >> multiple: aBoolean [
	multiple := aBoolean
]

{ #category : 'matching' }
ClapParameter >> newMatchAt: aStream in: aContext [
	^ ((self canMatchAt: aStream)
				ifTrue: [ self matchClass ]
				ifFalse: [ ClapMismatch ])
			of: self in: aContext
]

{ #category : 'printing' }
ClapParameter >> printOn: aStream [
	super printOn: aStream.
	aStream
		nextPut: $(;
		nextPutAll: self canonicalName;
		nextPut: $)
]

{ #category : 'documenting' }
ClapParameter >> synopsis [
	^ String streamContents: [ :aStream | self synopsisOn: aStream ]
]

{ #category : 'documenting' }
ClapParameter >> synopsisOn: aStream [
	^ self subclassResponsibility
]

{ #category : 'validation' }
ClapParameter >> validate: aMatch on: aReport [
	aReport addAll: (self validationDiagnosticsFor: aMatch)
]

{ #category : 'validation' }
ClapParameter >> validate: aClapValidationClass on: aReport match: aMatch [

	| diagnostics |
	diagnostics := aClapValidationClass
		ifNil: [ self validationDiagnosticsFor: aMatch ]
		ifNotNil: [ { aClapValidationClass new validate: aMatch } ].
	aReport addAll: (diagnostics reject: [ :diagnostic | diagnostic isSuccess ])
]

{ #category : 'validation' }
ClapParameter >> validationDiagnosticsFor: aMatch [
	^ validationBlock
		ifNil: [ #() ]
		ifNotNil: [ validationBlock value: aMatch ]
]

{ #category : 'initialization' }
ClapParameter >> validations: aBlock [
	validationBlock := aBlock
]

{ #category : 'accessing' }
ClapParameter >> valueFor: aMatch with: arg [
	^ self meaning
		ifNotNil: [ :meaning | meaning cull: aMatch cull: arg ]
]

{ #category : 'accessing' }
ClapParameter >> valueForImplicit: aMatch with: arg [
	^ self implicitMeaning ifNotNil: [ :block | block cull: aMatch cull: arg ]
]
