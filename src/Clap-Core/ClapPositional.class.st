"
I represent a free-form parameter which is passed as a single shell word and recognized based on its position in the input.

"
Class {
	#name : 'ClapPositional',
	#superclass : 'ClapParameter',
	#instVars : [
		'optional'
	],
	#category : 'Clap-Core-Specification',
	#package : 'Clap-Core',
	#tag : 'Specification'
}

{ #category : 'evaluating' }
ClapPositional class >> basicMeaning [
	^ [ :match | match word ]
]

{ #category : 'computing' }
ClapPositional class >> idFrom: aFlagId [

	^ aFlagId , '-value'
]

{ #category : 'adding' }
ClapPositional >> addTo: parentParameter [
	^ parentParameter addPositional: self
]

{ #category : 'api' }
ClapPositional >> beOptional [
	"The positional of a flag may be left out: the flag matches without it, and its value is then its default (see defaultValue:)"

	optional := true
]

{ #category : 'matching - testing' }
ClapPositional >> canMatchWith: word [
	^ (word beginsWith: '-') not
]

{ #category : 'api' }
ClapPositional >> defaultValue: aBlock [
	self implicitMeaning: aBlock
]

{ #category : 'accessing' }
ClapPositional >> flagId [

	"positional of a flag has an id like xxx-value"
	^ self identifier copyUpToLast: $-
]

{ #category : 'initialization' }
ClapPositional >> initialize [

	super initialize.
	optional := false
]

{ #category : 'api' }
ClapPositional >> isOptional [

	^ optional
]

{ #category : 'testing' }
ClapPositional >> isPositional [ 

	^ true
]

{ #category : 'accessing' }
ClapPositional >> matchClass [
	^ ClapWordMatch
]

{ #category : 'types' }
ClapPositional >> symbol [
	"tell that I must be converted as a symbol"
		
	self flag: 'TODO: convert as symbol'
]

{ #category : 'documenting' }
ClapPositional >> synopsisOn: aStream [

	self isOptional ifTrue: [ aStream nextPut: $[ ].
	aStream
		nextPut: $<;
		nextPutAll: self canonicalName;
		nextPut: $>.
	self isOptional ifTrue: [ aStream nextPut: $] ].
	^ aStream
]
