"
See the comment in NBExternalEnumeration. I am a copy of NBExternalEnumeration with the exception that I inherit from Object, not from SharedPool.
"
Class {
	#name : #LGitExternalEnumerationUInt32,
	#superclass : #Object,
	#instVars : [
		'value'
	],
	#classInstVars : [
		'representationType',
		'declaration'
	],
	#category : #'LibGit-Core-Abstracts'
}

{ #category : #converting }
LGitExternalEnumerationUInt32 class >> asExternalTypeOn: generator [
	^ FFIExternalEnumerationType 
		objectClass: self 
		representationType: self representationType
]

{ #category : #'class initialization' }
LGitExternalEnumerationUInt32 class >> buildDeclaration [
	| dictionary |
	self removeOldAccessors.
	dictionary := self literalDeclaration.
	self compileAccessorsFrom: dictionary.
	
	^ declaration := dictionary
]

{ #category : #'class initialization' }
LGitExternalEnumerationUInt32 class >> compileAccessorsFrom: aDictionary [
	aDictionary keysAndValuesDo: [ :symbol :value |
		self class
			compile: (String streamContents: [ :stream |
				stream
					nextPutAll: symbol asString; cr; tab;
					nextPutAll: '^ self basicNew'; cr; tab; tab;
					nextPutAll: 'value: ';
					nextPutAll: value asString;
					nextPutAll: ';'; cr; tab; tab;
					nextPutAll: 'yourself' ])
			classified: 'accessing-values' ]
]

{ #category : #accessing }
LGitExternalEnumerationUInt32 class >> declaration [
	^ declaration ifNil: [ declaration := self literalDeclaration ]
]

{ #category : #'accessing-defaults' }
LGitExternalEnumerationUInt32 class >> defaultRepresentationType [
	^ FFIUInt32 new
]

{ #category : #'enum declaration' }
LGitExternalEnumerationUInt32 class >> enumDecl [
	^#()
]

{ #category : #'instance creation' }
LGitExternalEnumerationUInt32 class >> fromInteger: anInteger [
	^self basicNew
		value: anInteger;
		yourself
]

{ #category : #testing }
LGitExternalEnumerationUInt32 class >> includes: aSymbol [
	^ self declaration includesKey: aSymbol
]

{ #category : #'class initialization' }
LGitExternalEnumerationUInt32 class >> initialize [
	declaration := nil.
	representationType := nil.
]

{ #category : #'enum declaration' }
LGitExternalEnumerationUInt32 class >> literalDeclaration [
	| dictionary |
	dictionary := self enumDecl in: [ :decl |
		decl isDictionary
			ifTrue: [ decl ]
			ifFalse: [ Dictionary newFromPairs: decl ] ].
		
	self makeSelectorsLegalIn: dictionary.
	^ dictionary
]

{ #category : #'enum declaration' }
LGitExternalEnumerationUInt32 class >> makeSelectorsLegalIn: aDictionary [
	aDictionary associationsDo: [ :nameToValue |
		| selector |
		selector := nameToValue key asLowercase asSymbol.
		nameToValue key: selector ]
]

{ #category : #'instance creation' }
LGitExternalEnumerationUInt32 class >> new [
	^ self shouldNotImplement 
]

{ #category : #'class initialization' }
LGitExternalEnumerationUInt32 class >> removeOldAccessors [
	declaration ifNil: [ ^ self ].
	
	declaration keys do: [ :selector |
		self class removeSelector: selector ]
]

{ #category : #accessing }
LGitExternalEnumerationUInt32 class >> representationType [
	^ representationType ifNil: [ representationType := self defaultRepresentationType ]
]

{ #category : #comparing }
LGitExternalEnumerationUInt32 >> = anEnumInst [

	^ self class == anEnumInst class and: [
		self value = anEnumInst value ]
]

{ #category : #comparing }
LGitExternalEnumerationUInt32 >> hash [
	^ self className hash bitXor: self value hash
]

{ #category : #testing }
LGitExternalEnumerationUInt32 >> isPartOfBitFlag: anInteger [
	"A bit flag may be composed of several values, where every bit
	represents the state of one flag. Therefore, a 2 bit value can
	represent 2 different flags, for instance. The #bitAnd: operation
	will mask all other flags.
	
	For some reason the libgit2 folks thought it would make sense to
	use flags of value 0 in bit flags. That forces one to check for zero
	(on the entire word)	explicitly. WTF."
	^ self value isZero
		ifTrue: [ anInteger isZero ]
		ifFalse: [ (self value bitAnd: anInteger) = self value ]
]

{ #category : #printing }
LGitExternalEnumerationUInt32 >> printOn: stream [
	super printOn: stream.

	value isInteger 		" temp: due to a uffi bug (?) sometimes value == self"
		ifFalse: [ ^ self ].

	stream nextPut: $(;
		nextPutAll: self symbol printString;
		nextPutAll: ' [';
		nextPutAll: self value asString;
		nextPut: $];
		nextPut: $)

]

{ #category : #accessing }
LGitExternalEnumerationUInt32 >> symbol [
	<todo: 'now we can handle enums which define the same value multiple times, but in case of this handler, we don''t know which name we need to associate with the return value. This is basically random...'>
	^ self class declaration
		keyAtValue: self value
		ifAbsent: [ nil ]
]

{ #category : #accessing }
LGitExternalEnumerationUInt32 >> value [
	^ value
]

{ #category : #accessing }
LGitExternalEnumerationUInt32 >> value: anInteger [
	value := anInteger 
]
