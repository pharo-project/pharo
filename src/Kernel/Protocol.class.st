"
A Protocol is a simple value holder representing a protocol.
It's composed of a name and a set of method selectors
"
Class {
	#name : #Protocol,
	#superclass : #AbstractProtocol,
	#instVars : [
		'name',
		'methodSelectors'
	],
	#category : #'Kernel-Protocols'
}

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

{ #category : #'adding-removing' }
Protocol >> addAllMethodsFrom: aProtocol [
	aProtocol methodSelectors do: [ :each | self addMethodSelector: each ]
]

{ #category : #'adding-removing' }
Protocol >> addMethodSelector: aSymbol [
	^ methodSelectors add: aSymbol
]

{ #category : #initialization }
Protocol >> initialize [

	super initialize.

	methodSelectors := IdentitySet new.
	name := self class defaultName
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
