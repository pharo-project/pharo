"
A Protocol is a cetagorization of methods in Pharo. 

A class has methods, and the methods are classified into protocols.

A class holds my instances (for now through ClassOrganization but we want to remove this class in the future) and I hold the selectors of the methods that are inside me.
I have a name that is used in browsers to display the different protocols.
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
Protocol class >> nullProtocolName [

	^ #'no messages'
]

{ #category : #accessing }
Protocol class >> unclassified [
	^ #'as yet unclassified'
]

{ #category : #'adding-removing' }
Protocol >> addAllMethodsFrom: aProtocol [
	aProtocol methodSelectors do: [ :each | self addMethodSelector: each ]
]

{ #category : #'adding-removing' }
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

	^ self methodSelectors includes: selector
]

{ #category : #initialization }
Protocol >> initialize [

	super initialize.

	methodSelectors := IdentitySet new.
	name := self class defaultName
]

{ #category : #testing }
Protocol >> isEmpty [
	^ self methodSelectors isEmpty
]

{ #category : #testing }
Protocol >> isExtensionProtocol [
	^ self name first = $*
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

{ #category : #'adding-removing' }
Protocol >> removeAllMethodSelectors [
	^ methodSelectors removeAll
]

{ #category : #'adding-removing' }
Protocol >> removeMethodSelector: aSymbol [
	^ methodSelectors remove: aSymbol
]

{ #category : #accessing }
Protocol >> rename: newName [

	self name: newName
]
