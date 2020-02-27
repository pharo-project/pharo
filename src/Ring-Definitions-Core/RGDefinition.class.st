"
I am the root class of the Ring meta-model.
A Ring definition has a name, can be annotated and knows its environment
"
Class {
	#name : #RGDefinition,
	#superclass : #Object,
	#instVars : [
		'annotations',
		'name'
	],
	#category : #'Ring-Definitions-Core-Base'
}

{ #category : #annotations }
RGDefinition class >> authorKey [

	^#author
]

{ #category : #annotations }
RGDefinition class >> fullNameKey [

	^#fullName
]

{ #category : #'class initialization' }
RGDefinition class >> named: aName [

	^self new
		name: aName asSymbol;
		yourself
]

{ #category : #annotations }
RGDefinition class >> timeStampKey [

	^#timestamp
]

{ #category : #annotations }
RGDefinition >> annotationNamed: annotationName [ 
	"Answer the value of the annotation named <annotationName>, or 
	nil if the annotation is not defined for the receiver."
	
	^ self
		annotationNamed: annotationName
		ifAbsent: [ nil ]
]

{ #category : #annotations }
RGDefinition >> annotationNamed: annotationName ifAbsent: exceptionBlock [ 
	"Answer the value of the annotation named <annotationName>. If the 
	annotation is not defined for the receiver, answer the result of 
	evaluating the <exceptionBlock>."

	self hasAnnotations
		ifFalse: [ ^exceptionBlock value ].

	^self annotations
			at: annotationName
			ifAbsent: [ exceptionBlock value ]
]

{ #category : #annotations }
RGDefinition >> annotationNamed: annotationName ifAbsentPut: blockValue [ 

	^self annotations
		at: annotationName
		ifAbsentPut: blockValue
]

{ #category : #annotations }
RGDefinition >> annotationNamed: annotationName put: value [ 

	self annotations
		at: annotationName
		put: value
]

{ #category : #annotations }
RGDefinition >> annotationNames [
	
	self hasAnnotations
		ifFalse:[ ^OrderedCollection new ].
	^annotations keys
]

{ #category : #annotations }
RGDefinition >> annotations [

	^annotations ifNil:[ annotations:= IdentityDictionary new ]
]

{ #category : #converting }
RGDefinition >> asRingDefinition [
	^ self
]

{ #category : #accessing }
RGDefinition >> environment [
	"If the receiver has not namespace assigned the default is used"
	"self class environment = Smalltalk globals"

	^ self annotationNamed: #environment ifAbsent: [ self class environment ]
]

{ #category : #accessing }
RGDefinition >> environment: namespace [

	namespace = self class environment
		ifFalse:[ self annotationNamed: #environment put: namespace ]
]

{ #category : #accessing }
RGDefinition >> fullName [

	^self name
]

{ #category : #annotations }
RGDefinition >> hasAnnotationNamed: annotationName [
	"Answer <true> if the receiver contains the annotation named <annotationName>. "
	
	self hasAnnotations	
		ifFalse:[ ^false ].
	^annotations includesKey: annotationName
]

{ #category : #annotations }
RGDefinition >> hasAnnotations [

	^annotations notNil
]

{ #category : #'testing types' }
RGDefinition >> isAccess [ 

	^false
]

{ #category : #'testing types' }
RGDefinition >> isComment [

	^false
]

{ #category : #'testing types' }
RGDefinition >> isGlobalVariable [

	^false
]

{ #category : #'testing types' }
RGDefinition >> isInheritance [

	^false
]

{ #category : #'testing types' }
RGDefinition >> isInvocation [

	^false
]

{ #category : #'testing types' }
RGDefinition >> isMethod [

	^false
]

{ #category : #'testing types' }
RGDefinition >> isNamespace [

	^false
]

{ #category : #'testing types' }
RGDefinition >> isOrganization [ 

	^false
]

{ #category : #'testing types' }
RGDefinition >> isPackage [

	^false
]

{ #category : #'testing types' }
RGDefinition >> isPool [

	^false
]

{ #category : #'testing types' }
RGDefinition >> isReference [ 

	^false
]

{ #category : #'testing types' }
RGDefinition >> isRingObject [

	^true
]

{ #category : #'testing types' }
RGDefinition >> isSlice [

	^false
]

{ #category : #'testing types' }
RGDefinition >> isVariable [

	^false
]

{ #category : #accessing }
RGDefinition >> name [

	^name
]

{ #category : #accessing }
RGDefinition >> name: anObject [
	"a symbol or a string should be expected"
	
	name:= anObject
]

{ #category : #annotations }
RGDefinition >> removeAnnotationNamed: annotationName [ 
	"Remove the annotation named <annotationName>. Fails quietly if there 
	is no annotation with the given name."
	
	(self hasAnnotationNamed: annotationName)
		ifTrue:[ annotations removeKey: annotationName ]
]

{ #category : #accessing }
RGDefinition >> rootEnvironment [
	"Retrieves the runtime environment of an RGObject"
	"If the receiver is defined in an RGContainer the runtime environment is the one associated to its container"
	| parent |
	
	parent:= self environment.
	[ parent isRingObject ]
		whileTrue:[ parent:= parent environment ].

	^parent
]
