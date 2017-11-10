"
A Protocol is a simple value holder representing a protocol.
It's composed of a name and a set of method selectors
"
Class {
	#name : #Protocol,
	#superclass : #Object,
	#instVars : [
		'methods',
		'name'
	],
	#category : #Kernel-Protocols
}

{ #category : #'instance creation' }
Protocol class >> ambiguous [
	^ #ambiguous
]

{ #category : #accessing }
Protocol class >> defaultName [

	^  self unclassified
]

{ #category : #'instance creation' }
Protocol class >> empty [ 
	 ^ self name: #''
]

{ #category : #'instance creation' }
Protocol class >> name: nm [

	^ self new
		name: nm;
		yourself
]

{ #category : #'instance creation' }
Protocol class >> name: nm methods: methods [ 

	^ self new
		methods: methods;
		name: nm;
		yourself
]

{ #category : #accessing }
Protocol class >> nullCategory [

	^ #'no messages'
]

{ #category : #accessing }
Protocol class >> unclassified [
	^ #'as yet unclassified'
]

{ #category : #accessing }
Protocol >> addAllMethodsFrom: aProtocol [

	aProtocol methods do: [ :each | self addMethod: each ].
]

{ #category : #accessing }
Protocol >> addMethod: aSymbol [
	^ methods add: aSymbol
]

{ #category : #private }
Protocol >> canBeRemoved [
	^ self isEmpty 
]

{ #category : #testing }
Protocol >> canBeRenamed [
	^ true
]

{ #category : #testing }
Protocol >> includesSelector: selector [

	^ methods includes: selector
]

{ #category : #initialization }
Protocol >> initialize [

	super initialize.

	methods := IdentitySet new.
	name := self class defaultName.
]

{ #category : #testing }
Protocol >> isEmpty [

	^ self methods isEmpty
]

{ #category : #testing }
Protocol >> isExtensionProtocol [ 
	^ self name first = $*.
]

{ #category : #testing }
Protocol >> isVirtualProtocol [
	"A virtual protocol is a calculated one (it does not have any methods by it self)"
	^ false
]

{ #category : #accessing }
Protocol >> methods [

	^ methods
]

{ #category : #accessing }
Protocol >> methods: anObject [
	
	methods := anObject
]

{ #category : #accessing }
Protocol >> name [

	^ name
]

{ #category : #accessing }
Protocol >> name: anObject [
	name := anObject asSymbol
]

{ #category : #printing }
Protocol >> printOn: aStream [

	aStream 
		nextPutAll: (self class name);
		nextPutAll: ' (';
		nextPutAll: (self name);
		nextPutAll: ') - ';
		nextPutAll: (self methods size asString);
		nextPutAll: ' selector(s)'.
]

{ #category : #accessing }
Protocol >> removeAllMethods [

	^ methods removeAll.
]

{ #category : #accessing }
Protocol >> removeMethod: aSymbol [

	^ methods remove: aSymbol
]

{ #category : #accessing }
Protocol >> rename: newName [

	self name: newName
]
