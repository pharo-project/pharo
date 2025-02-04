"
A Protocol is a categorization of methods in Pharo. 
A class has methods, and the methods are classified into protocols. We currently have 1 protocol by method.
A class holds my instances and I hold the selectors of the methods that are inside me.
I have a name that is used in browsers to display the different protocols and I know which class owns me.
"
Class {
	#name : 'Protocol',
	#superclass : 'Object',
	#instVars : [
		'name',
		'methodSelectors',
		'owningClass'
	],
	#category : 'Kernel-CodeModel-Classes',
	#package : 'Kernel-CodeModel',
	#tag : 'Classes'
}

{ #category : 'instance creation' }
Protocol class >> name: aString methodSelectors: methods [

	^ (self named: aString)
		  methodSelectors: methods;
		  yourself
]

{ #category : 'instance creation' }
Protocol class >> named: aStrings [

	^ self new
		  name: aStrings;
		  yourself
]

{ #category : 'instance creation' }
Protocol class >> named: aString class: aClass [

	^ (self named: aString)
		  owningClass: aClass;
		  yourself
]

{ #category : 'accessing' }
Protocol class >> unclassified [
	^ #'as yet unclassified'
]

{ #category : 'adding-removing' }
Protocol >> addAllMethodsFrom: aProtocol [
	aProtocol methodSelectors do: [ :each | self addMethodSelector: each ]
]

{ #category : 'adding-removing' }
Protocol >> addMethodSelector: aSymbol [
	(methodSelectors includes: aSymbol) ifTrue: [ ^ self ].
	
	methodSelectors := methodSelectors copyWith: aSymbol
]

{ #category : 'testing' }
Protocol >> includesSelector: selector [

	^ self methodSelectors includes: selector
]

{ #category : 'initialization' }
Protocol >> initialize [

	super initialize.
	
	self resetMethodSelectors.
	name := self class unclassified
]

{ #category : 'testing' }
Protocol >> isEmpty [
	^ self methodSelectors isEmpty
]

{ #category : 'testing' }
Protocol >> isExtensionProtocol [
	"For now, extension methods are managed via protocols. 
	
	The protocol needs to be '*NameOfThePackage'. This is an implementation detail and in the future we want to manage extensions in a different way to not rely on protocols. But until now, I can be used to know if I am used to manage extension methods."

	^ self name first = $*
]

{ #category : 'testing' }
Protocol >> isExtensionProtocolMatching: aPackage [
	"Return true if I am an extension protocol linked to the package as parameter"

	^ (aPackage organizer packageForProtocol: self) = aPackage
]

{ #category : 'testing' }
Protocol >> isUnclassifiedProtocol [

	^ self name = self class unclassified
]

{ #category : 'accessing' }
Protocol >> methodSelectors [
	^ methodSelectors
]

{ #category : 'accessing' }
Protocol >> methodSelectors: anObject [
	methodSelectors := anObject
]

{ #category : 'accessing' }
Protocol >> name [

	^ name
]

{ #category : 'accessing' }
Protocol >> name: anObject [
	name := anObject asSymbol
]

{ #category : 'accessing' }
Protocol >> owningClass [
	"The owning class is the class in which I am defined."

	^ owningClass
]

{ #category : 'accessing' }
Protocol >> owningClass: aClass [

	owningClass := aClass
]

{ #category : 'printing' }
Protocol >> printOn: aStream [
	aStream
		nextPutAll: self class name;
		nextPutAll: ' (';
		nextPutAll: self name;
		nextPutAll: ') - ';
		print: self methodSelectors size;
		nextPutAll: ' selector(s)'
]

{ #category : 'adding-removing' }
Protocol >> removeMethodSelector: aSymbol [
	methodSelectors := methodSelectors copyWithout: aSymbol
]

{ #category : 'accessing' }
Protocol >> rename: newName [

	self name: newName
]

{ #category : 'adding-removing' }
Protocol >> resetMethodSelectors [
	methodSelectors := Array empty
]
