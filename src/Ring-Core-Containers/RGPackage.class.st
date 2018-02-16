"
RGPackage is the representation for packages.
A package manages classes, methods and children packages as elements.
A package can also know in which package is defined

"
Class {
	#name : #RGPackage,
	#superclass : #RGContainer,
	#instVars : [
		'package'
	],
	#category : #'Ring-Core-Containers'
}

{ #category : #'adding/removing' }
RGPackage >> addElement: aRGDefinition in: aCollection [

	aRGDefinition package: self.
	super addElement: aRGDefinition in: aCollection.
]

{ #category : #'managing container' }
RGPackage >> addInContainer: anRGContainer [

	anRGContainer addPackage: self
]

{ #category : #'adding/removing' }
RGPackage >> addMethod: aRGMethodDefinition [
	"aRGMethodDefinition needs to satisfy the status isMethod"

	aRGMethodDefinition isMethod ifFalse:[ ^self ].
	aRGMethodDefinition package: self.	"Needed to evaluate #isExtension if it was not set in advance"
	self 
		addElement: aRGMethodDefinition 
		in: (aRGMethodDefinition isExtension 
			ifTrue: [ self extensionMethods ]
			ifFalse:[ super methods ])
]

{ #category : #accessing }
RGPackage >> categories [
	"Retrieves class categories of the receiver"
	
	^self elements at: #categories
]

{ #category : #accessing }
RGPackage >> categories: aCollection [
	"Set the class categories collection"
	
	self elements at: #categories put: (aCollection asOrderedCollection)
]

{ #category : #accessing }
RGPackage >> classNames [

	^ self classes isDictionary
		ifTrue: [ self classes keys ]
		ifFalse:[ self classes collect:[ :class|  class name ] ]	
]

{ #category : #accessing }
RGPackage >> classes [
	"Retrieves a collection (by default a dictionary) of classes defined in the receiver and classes holding extension methods"
	
	^self definedClasses, self extendedClasses 
]

{ #category : #accessing }
RGPackage >> definedClassNames [

	^ super classNames
]

{ #category : #accessing }
RGPackage >> definedTraits [
	"Retrieves the traits defined in the receiver"

	^self traits
]

{ #category : #testing }
RGPackage >> definesClass: aRGBehaviorDefinition [
	"Returns true if the receiver includes aRGBehaviorDefinition in the defined classes"

	^ self includesClass: aRGBehaviorDefinition
]

{ #category : #testing }
RGPackage >> definesOrExtendsClass: aRGBehaviorDefinition [
	"Returns true whether the class, aRGBehaviorDefinition, is one locally defined classes of the receiver or 
	if the receiver extends such class (that is defined in another package)"
	
	^ (self definesClass: aRGBehaviorDefinition) or:[ self extendsClass: aRGBehaviorDefinition ]
]

{ #category : #accessing }
RGPackage >> extendedClassNames [

	^ self extendedClasses isDictionary
		ifTrue: [ self extendedClasses keys ]
		ifFalse:[ self extendedClasses collect:[ :each| each name ]]
]

{ #category : #accessing }
RGPackage >> extendedClasses [
	"Retrieves the classes holding the extension methods and returns a dictionary as well"
	|  classesOfExt |
	classesOfExt := Dictionary new.
	
	self extensionMethods
		do:[ :mth| (classesOfExt includesKey: mth parentName)
					ifFalse:[ classesOfExt at: mth parentName put: mth parent ] ].
	
	^classesOfExt
]

{ #category : #accessing }
RGPackage >> extendedSelectors [
	"Retrieves the names of the methods"
	
	^ self extensionMethods keys
]

{ #category : #testing }
RGPackage >> extendsClass: aRGBehaviorDefinition [
	"Returns true if the receiver extends aRGBehaviorDefinition (that is defined in another package)"
	
	^ self extendedClasses includes: aRGBehaviorDefinition
]

{ #category : #accessing }
RGPackage >> extensionMethods [
	"Retrieves extension methods"
	
	^self elements at: #extensionMethods
]

{ #category : #accessing }
RGPackage >> extensionMethods: aCollection [
	"Set the extension methods collection"
	
	self elements at: #extensionMethods put: aCollection
]

{ #category : #initialization }
RGPackage >> initialize [

	super initialize.
	self withoutPackages.	"By default the receiver has no child packages"
	self extensionMethods: IdentityDictionary new.		
	self categories: OrderedCollection new.
]

{ #category : #testing }
RGPackage >> isPackage [

	^true
]

{ #category : #lookup }
RGPackage >> lookupClassNamed: className [
	"Look for the class named className in the receiver's defined classes or in any of its package children"
	| class |
	
	class:= self classNamed: className.
	class ifNotNil:[ ^class ].
	
	self packages
		do:[ :each| class:= each lookupClassNamed: className.
				    class ifNotNil:[ ^class ] ].
	^nil
]

{ #category : #accessing }
RGPackage >> methods [
	"Retrieves all the methods defined in the receiver. 
	#methods holds the methods of defined classes"
	
	^super methods, self extensionMethods 
]

{ #category : #metrics }
RGPackage >> numberOfLinesOfCode [
	
	^ self 
		annotationNamed: #numberOfLinesOfCode
		ifAbsentPut: [
			| number |
			number := self classes inject: 0 into: [ :sum :each | sum + each numberOfLinesOfCode + each classSide numberOfLinesOfCode ].
			number := self methods inject: number into: [ :sum :each | sum + each numberOfLinesOfCode ].
			number ]
]

{ #category : #accessing }
RGPackage >> package [
	"Retrieves the  package in which self is contained"

	^package
]

{ #category : #accessing }
RGPackage >> package: aRGPackage [
	"Sets the outer package of the receiver if exists"

	package:= aRGPackage
]

{ #category : #accessing }
RGPackage >> parent [
	"Retrieves the environment linked to this package"
	
	^self environment
]

{ #category : #printing }
RGPackage >> printOn: aStream [
	aStream
		nextPutAll: self class name;
		nextPutAll: '(';
		nextPutAll: self name;
		nextPutAll: ')'
]

{ #category : #accessing }
RGPackage >> realPackage [

	^ RPackageOrganizer default packageNamed: self name
]

{ #category : #'adding/removing' }
RGPackage >> removeMethod: aRGMethodDefinition [

	self 
		removeElement: aRGMethodDefinition 
		from: (aRGMethodDefinition isExtension 
				ifTrue: [ self extensionMethods ]
				ifFalse:[ super methods ])
]

{ #category : #'adding/removing' }
RGPackage >> withoutCategories [

	self removeElementsCategorized: #categories
]
