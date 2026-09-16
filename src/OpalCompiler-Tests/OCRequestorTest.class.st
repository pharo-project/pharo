Class {
	#name : 'OCRequestorTest',
	#superclass : 'TestCase',
	#instVars : [
		'requestorBlock'
	],
	#category : 'OpalCompiler-Tests-Requestor',
	#package : 'OpalCompiler-Tests',
	#tag : 'Requestor'
}

{ #category : 'tests' }
OCRequestorTest >> bindingOf: aName [

	^ nil
]

{ #category : 'tests' }
OCRequestorTest >> canAddBindingOf: aName [
	"yup"

	^ true
]

{ #category : 'tests' }
OCRequestorTest >> hasBindingOf: aName [
	"Nope"

	^ false
]

{ #category : 'tests' }
OCRequestorTest >> lookupVar: aName [

	^ requestorBlock value: aName
]

{ #category : 'tests' }
OCRequestorTest >> needRequestorScope [

	"Oh yeah"
	^ true
]

{ #category : 'tests' }
OCRequestorTest >> notify: aString at: anInteger in: aStream [

	"Nothing..."
]

{ #category : 'tests' }
OCRequestorTest >> testRequestorReturningUndeclaredCompiles [

	| compiler method |
	self whenRequestingVariableDo: [ :name | UndeclaredVariable named: name ].

	compiler := OpalCompiler new
		            requestor: self;
		            yourself.
	method := compiler compile: 'toto
		| accum |
		accum := 0.
		1 ifNotNil: [ :one | accum := accum + one ].
		3 ifNotNil: [ :one | accum := accum + one ].
		^ accum'.

	self assert: (method valueWithReceiver: nil) equals: 4
	
]

{ #category : 'tests' }
OCRequestorTest >> whenRequestingVariableDo: aBlock [

	requestorBlock := aBlock
]
