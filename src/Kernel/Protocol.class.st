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
Protocol class >> name: aString [

	self deprecated: 'Use #named: instead' transformWith: '`@rcv name: `@arg' -> '`@rcv named: `@arg'.
	^ self named: aString
]

{ #category : #'instance creation' }
Protocol class >> name: aString methodSelectors: methods [

	^ (self named: aString)
		  methodSelectors: methods;
		  yourself
]

{ #category : #'instance creation' }
Protocol class >> named: aStrings [

	^ self new
		  name: aStrings;
		  yourself
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
	(methodSelectors includes: aSymbol) ifTrue: [ ^ self ].
	
	methodSelectors := methodSelectors copyWith: aSymbol
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
	
	self resetMethodSelectors.
	name := self class unclassified
]

{ #category : #testing }
Protocol >> isEmpty [
	^ self methodSelectors isEmpty
]

{ #category : #testing }
Protocol >> isExtensionProtocol [
	^ self name first = $*
]

{ #category : #testing }
Protocol >> isUnclassifiedProtocol [

	^ self name = self class unclassified
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

{ #category : #'adding-removing' }
Protocol >> removeMethodSelector: aSymbol [
	methodSelectors := methodSelectors copyWithout: aSymbol
]

{ #category : #accessing }
Protocol >> rename: newName [

	self name: newName
]

{ #category : #'adding-removing' }
Protocol >> resetMethodSelectors [
	methodSelectors := Array empty
]
