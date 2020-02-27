"
RGClassDescriptionDefinition is the common parent for classes and metaclasses defining instance variables
"
Class {
	#name : #RGClassDescriptionDefinition,
	#superclass : #RGBehaviorDefinition,
	#instVars : [
		'instanceVariables',
		'organization'
	],
	#category : #'Ring-Definitions-Core-Base'
}

{ #category : #comparing }
RGClassDescriptionDefinition >> = aRGClassDefinition [

	^self class = aRGClassDefinition class 
		and: [ self name = aRGClassDefinition name 
			and: [ self environment = aRGClassDefinition environment ] ]
]

{ #category : #'instance variables' }
RGClassDescriptionDefinition >> addInstVarNamed: aString [
	| var |
	var := ((self isMeta 
				ifTrue: [ RGClassInstanceVariableDefinition ]
				ifFalse:[  RGInstanceVariableDefinition ])
		named: aString) parent: self.
	self addVariable: var in: instanceVariables.
	^var
]

{ #category : #'instance variables' }
RGClassDescriptionDefinition >> addInstanceVariable: aRGVariableDefinition [
	"aRGVariableDefinition is a instance variable or class instance variable"

	self addVariable: (aRGVariableDefinition parent: self)
		in: instanceVariables
]

{ #category : #'instance variables' }
RGClassDescriptionDefinition >> addInstanceVariables: aCollection [
	
	aCollection do: [:var | self addInstVarNamed: var ]
]

{ #category : #private }
RGClassDescriptionDefinition >> addVariable: aRGVariableDefinition in: aCollection [
	"Adds a RGVariableDefinition in the collection received" 

	aCollection add: aRGVariableDefinition
]

{ #category : #'instance variables' }
RGClassDescriptionDefinition >> allInstVarNames [

	^self allInstanceVariables collect:[ :ivar| ivar name ]
]

{ #category : #'instance variables' }
RGClassDescriptionDefinition >> allInstanceVariables [
	"Answer a collection of the receiver's instanceVariables, including those defined in its superclass"

	^self hasSuperclass 
		ifFalse:[ instanceVariables ]
		ifTrue:[ self superclass allInstanceVariables, instanceVariables ]
]

{ #category : #accessing }
RGClassDescriptionDefinition >> extendingPackages [

	^ self realClass extendingPackages
]

{ #category : #comparing }
RGClassDescriptionDefinition >> hash [

	^self name hash bitXor: self class hash
]

{ #category : #initialization }
RGClassDescriptionDefinition >> initialize [

	super initialize.
	instanceVariables:= OrderedCollection new.
]

{ #category : #'instance variables' }
RGClassDescriptionDefinition >> instVarNames [
	"Answer a collection of the names of the instance variables defined in the receiver."
	
	^instanceVariables collect:[ :ivar| ivar name ]
]

{ #category : #'instance variables' }
RGClassDescriptionDefinition >> instanceVariableNamed: aString [
	^ instanceVariables 
		detect: [ :v | v name = aString asSymbol ] 
		ifNone: [ nil ]
]

{ #category : #accessing }
RGClassDescriptionDefinition >> instanceVariables [

	^instanceVariables
]

{ #category : #accessing }
RGClassDescriptionDefinition >> instanceVariables: aCollection [

	instanceVariables:= aCollection
]

{ #category : #testing }
RGClassDescriptionDefinition >> isClass [

	^true
]

{ #category : #testing }
RGClassDescriptionDefinition >> isSameRevisionAs: aRGClassDescriptionDefinition [
	"This method look for equality of the properties of the receiver"
	"Instances variables are compared at the level of names but without any sorting"

	^(super isSameRevisionAs: aRGClassDescriptionDefinition)
		and:[ self instVarNames sort = aRGClassDescriptionDefinition instVarNames sort ]
]

{ #category : #organization }
RGClassDescriptionDefinition >> organization [
	"Answer the instance of ClassOrganizer that represents the organization 
	of the messages of the receiver."

	organization ifNil: [
		self organization: (ClassOrganization forClass: self) ].
	"Making sure that subject is set correctly. It should not be necessary."
	organization ifNotNil: [ organization setSubject: self ].
	^ organization
]

{ #category : #accessing }
RGClassDescriptionDefinition >> organization: aClassOrg [
	"Install an instance of ClassOrganizer that represents the organization of the messages of the receiver."

	aClassOrg ifNotNil: [aClassOrg setSubject: self].
	organization := aClassOrg
]

{ #category : #'instance variables' }
RGClassDescriptionDefinition >> removeInstVarNamed: aString [

	self removeVariable: (self instanceVariableNamed: aString) from: instanceVariables
]

{ #category : #'instance variables' }
RGClassDescriptionDefinition >> removeInstanceVariable: aRGVariableDefinition [
	"aRGVariableDefinition is a instance variable or class instance variable"

	self removeVariable: aRGVariableDefinition from: instanceVariables
]

{ #category : #private }
RGClassDescriptionDefinition >> removeVariable: aRGVariableDefinition from: aCollection [
	"Removes a variable from a particular collection.
	This behavior is the same for any kind of variable"
	aCollection remove: aRGVariableDefinition ifAbsent:[]
]

{ #category : #printing }
RGClassDescriptionDefinition >> storeOn: aStream [
	aStream
		nextPutAll: '(';
		nextPutAll: self class name;
		nextPutAll: ' named: '.
	name storeOn: aStream.
	aStream nextPut: $)
]
