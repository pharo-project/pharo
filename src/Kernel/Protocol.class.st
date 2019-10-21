"
A Protocol is a simple value holder representing a protocol.
It's composed of a name and a set of method selectors
"
Class {
	#name : #Protocol,
	#superclass : #Object,
	#instVars : [
		'name',
		'methodSelectors'
	],
	#category : #'Kernel-Protocols'
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
		name: nm asSymbol;
		yourself
]

{ #category : #'instance creation' }
Protocol class >> name: nm methodSelectors: methods [
	^ self new
		methodSelectors: methods;
		name: nm asSymbol;
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
	aProtocol methodSelectors do: [ :each | self addMethodSelector: each ]
]

{ #category : #accessing }
Protocol >> addMethodSelector: aSymbol [
	^ methodSelectors add: aSymbol
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

	^ methodSelectors includes: selector
]

{ #category : #initialization }
Protocol >> initialize [

	super initialize.

	methodSelectors := IdentitySet new.
	name := self class defaultName.
]

{ #category : #testing }
Protocol >> isEmpty [
	^ self methodSelectors isEmpty
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
Protocol >> methodSelectors [
	^ methodSelectors
]

{ #category : #accessing }
Protocol >> methodSelectors: anObject [
	methodSelectors := anObject
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
		nextPutAll: self class name;
		nextPutAll: ' (';
		nextPutAll: self name;
		nextPutAll: ') - ';
		print: self methodSelectors size;
		nextPutAll: ' selector(s)'
]

{ #category : #accessing }
Protocol >> removeAllMethodSelectors [
	^ methodSelectors removeAll
]

{ #category : #accessing }
Protocol >> removeMethodSelector: aSymbol [
	^ methodSelectors remove: aSymbol
]

{ #category : #accessing }
Protocol >> rename: newName [

	self name: newName
]
