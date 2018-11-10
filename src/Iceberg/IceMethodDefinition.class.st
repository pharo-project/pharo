"
I represent a method read from an Iceberg repository, either from a commit or from the working directory in disk.

My contents is the code of the method.

See my superclasses for more information.
"
Class {
	#name : #IceMethodDefinition,
	#superclass : #IceCodeDefinition,
	#instVars : [
		'className',
		'classIsMeta',
		'protocol',
		'sourceCode'
	],
	#category : #'Iceberg-Changes'
}

{ #category : #'instance creation' }
IceMethodDefinition class >> className: aClassName classIsMeta: classIsMeta protocol: protocol name: aString sourceCode: source [

	^ (self named: aString)
		className: aClassName;
		classIsMeta: classIsMeta;
		protocol: protocol;
		sourceCode: source;
		yourself
]

{ #category : #comparing }
IceMethodDefinition >> = anotherDefinition [
	"A method can be equal and belong to another class (and be in the other side). 
	 And yes, even if this usually should not happen, it happens (and not always is incorrect)"
	^ super = anotherDefinition 
		and: [ self className = anotherDefinition className 
		and: [ self classIsMeta = anotherDefinition classIsMeta ] ]
]

{ #category : #visiting }
IceMethodDefinition >> accept: aVisitor [
	
	^ aVisitor visitMethodNode: self
]

{ #category : #'morphic-compatibility' }
IceMethodDefinition >> actualClass [

	^ self contextClass
]

{ #category : #patching }
IceMethodDefinition >> addToPatcher: aMCPatcher [ 
	
	aMCPatcher addDefinition: self asMCDefinition
]

{ #category : #patching }
IceMethodDefinition >> asMCDefinition [

	^ MCMethodDefinition 
		className: self className
		classIsMeta: self classIsMeta
		selector: self name
		category: self protocol
		timeStamp: ''
		source: sourceCode
]

{ #category : #accessing }
IceMethodDefinition >> classIsMeta [
	^ classIsMeta
]

{ #category : #accessing }
IceMethodDefinition >> classIsMeta: anObject [
	classIsMeta := anObject
]

{ #category : #accessing }
IceMethodDefinition >> className [

	^ className
]

{ #category : #accessing }
IceMethodDefinition >> className: anObject [
	className := anObject
]

{ #category : #accessing }
IceMethodDefinition >> contents [

	^ '"', self protocol, '"
', sourceCode
]

{ #category : #'morphic-compatibility' }
IceMethodDefinition >> contextClass [
	
	| class |
	class := Smalltalk globals at: self className ifAbsent: [ ^ nil ].
	self classIsMeta ifTrue: [ class := class classSide ].
	^ class
]

{ #category : #actions }
IceMethodDefinition >> install [
	
	self asMCDefinition load
]

{ #category : #'morphic-compatibility' }
IceMethodDefinition >> isLoadable [
	^ self contextClass notNil
]

{ #category : #testing }
IceMethodDefinition >> isMethodDefinition [
	
	^ true
]

{ #category : #accessing }
IceMethodDefinition >> ownerName [
	
	^ className
]

{ #category : #accessing }
IceMethodDefinition >> protocol [
	^ protocol
]

{ #category : #accessing }
IceMethodDefinition >> protocol: anObject [
	protocol := anObject
]

{ #category : #patching }
IceMethodDefinition >> removeFrom: aPatcher [
	
	aPatcher removeMethod: self
]

{ #category : #patching }
IceMethodDefinition >> removeFromPatcher: aMCPatcher [ 
	
	aMCPatcher removeDefinition: self asMCDefinition
]

{ #category : #accessing }
IceMethodDefinition >> selector [
	
	^ name
]

{ #category : #accessing }
IceMethodDefinition >> sourceCode [

	^ sourceCode
]

{ #category : #accessing }
IceMethodDefinition >> sourceCode: aString [

	sourceCode := aString
]
