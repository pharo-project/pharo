"
I am a RBBrowserEnvironment on a set of protocols of a class.

Example:
(RBBrowserEnvironment new) forClass:Morph protocols:{'printing'}.
"
Class {
	#name : 'RBProtocolEnvironment',
	#superclass : 'RBBrowserEnvironmentWrapper',
	#instVars : [
		'class',
		'protocols'
	],
	#category : 'Refactoring-Environment',
	#package : 'Refactoring-Environment'
}

{ #category : 'instance creation' }
RBProtocolEnvironment class >> class: aClass protocols: aCollection [
	^ self
		onEnvironment: self default
		class: aClass
		protocols: aCollection
]

{ #category : 'accessing - defaults' }
RBProtocolEnvironment class >> defaultName [
	^ 'Protocols'
]

{ #category : 'instance creation' }
RBProtocolEnvironment class >> onEnvironment: anEnvironment class: aClass protocols: aCollection [
	^(self onEnvironment: anEnvironment)
		class: aClass protocols: aCollection; yourself
]

{ #category : 'comparing' }
RBProtocolEnvironment >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	self class = anObject class ifFalse: [ ^ false ].
	^ environment = anObject environment and: [
		  class = anObject definedClass and: [
			  protocols = anObject protocols ] ]
]

{ #category : 'adding' }
RBProtocolEnvironment >> addProtocol: aSymbol [
	protocols add: aSymbol
]

{ #category : 'accessing' }
RBProtocolEnvironment >> basisObjects [
	^ self protocols
]

{ #category : 'initialization' }
RBProtocolEnvironment >> class: aClass protocols: aCollection [
	class := aClass.
	protocols := aCollection asOrderedCollection
]

{ #category : 'private' }
RBProtocolEnvironment >> defaultLabel [
	| stream |
	stream := String new writeStream.
	stream nextPutAll: class name;
		nextPut: $>.
	protocols do:
			[:each |
			stream nextPutAll: each;
				nextPut: $ ].
	^stream contents
]

{ #category : 'accessing' }
RBProtocolEnvironment >> definedClass [
	^ class
]

{ #category : 'testing' }
RBProtocolEnvironment >> definesClass: aClass [
	^ aClass == class and: [super definesClass: aClass]
]

{ #category : 'description' }
RBProtocolEnvironment >> description [

	^ label ifNil: [self defaultName , ' of ', self class name, self descriptionBasis]
]

{ #category : 'comparing' }
RBProtocolEnvironment >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ class hash bitXor: protocols hash
]

{ #category : 'testing' }
RBProtocolEnvironment >> includesClass: aClass [
	^ aClass == class and: [super includesClass: aClass]
]

{ #category : 'testing' }
RBProtocolEnvironment >> includesProtocol: aProtocol in: aClass [

	^ aClass == class and: [ (super includesProtocol: aProtocol in: aClass) and: [ protocols includes: aProtocol name ] ]
]

{ #category : 'testing' }
RBProtocolEnvironment >> includesSelector: aSelector in: aClass [
	^(super includesSelector: aSelector in: aClass) and:
			[protocols
				includes: (environment whichProtocolIncludes: aSelector in: aClass)]
]

{ #category : 'testing' }
RBProtocolEnvironment >> isEmpty [
	^protocols isEmpty
]

{ #category : 'copying' }
RBProtocolEnvironment >> postCopy [
	super postCopy.
	protocols := protocols copy
]

{ #category : 'printing' }
RBProtocolEnvironment >> printOn: aStream [

	aStream nextPutAll: class name, ' protocols: '.
	protocols do: [ :cls | aStream nextPutAll: cls, ', ']
]

{ #category : 'accessing' }
RBProtocolEnvironment >> protocols [

	^ protocols
]

{ #category : 'removing' }
RBProtocolEnvironment >> removeProtocol: aSymbol [
	protocols remove: aSymbol ifAbsent: [  ]
]

{ #category : 'storing' }
RBProtocolEnvironment >> storeOn: aStream [
	aStream nextPut: $(.
	super storeOn: aStream.
	aStream
		nextPutAll: ' class: ';
		nextPutAll: class name;
		nextPutAll: ' protocols: '.
	protocols asArray storeOn: aStream.
	aStream nextPut: $)
]
