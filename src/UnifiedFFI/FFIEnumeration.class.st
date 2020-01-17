"
Enumerations are data-types defining a finite set of named values.
For example, let's consider we want to create a data-type to identify the different positions of players inside a football match: goalkeeper, defender, midfielder, forward. Such data-type can be defined in C as an enumeration as follows:

[[[language=c
typedef enum {
  goalkeeper,
  defender,
  midfielder,
  forward
} position;
]]]

We can then use ==position== as a type, and the values defined within it as valid values for ==position==.


[[[language=c
position myPosition = defender;
]]]

!!!! The values of C enumerations

To better understand how to map C enumerations using uFFI, we must before understand how C assigns value to each of the elements in the enumeration.
Internally, C assigns to each of the elements of the enumeration a sequencial numeric value starting from 0 (zero).
In other words, ==goalkeeper== has a value of 0, ==defender== has a value of 1, and so on.
C allows developers to specify the values they want too, using an assignment-like syntax.

[[[language=c
typedef enum {
  goalkeeper = 42,
  defender,
  midfielder,
  forward
} position;
]]]

We can explicitly assign values to any of the elements of the enumeration.
We may leave values without explicit values, in which case they will be automatically assigned a value following its previous value. And finally, many elements in the enumeration may have the same value.
The example enumeration below shows these subtleties.

[[[language=c
#include <assert.h>
#include <limits.h>

enum example {
    example0,            /* will have value 0 */
    example1,            /* will have value 1 */
    example2 = 3,        /* will have value 3 */
    example3 = 3,        /* will have value 3 */
    example4,            /* will have value 4 */
    example5 = INT_MAX,  /* will have value INT_MAX */
    /* Defining a new value after this one will cause an overflow error */
};
]]]

!!!! Defining an enumeration using ==FFIEnumeration==

Enumerations are declared in uFFI as subclasses of the ==FFIEnumeration== class defining the same elements as defined in C, and with their same values.
For example, defining our example enumeration is done as follows, defining a subclass of ==FFIEnumeration==, a ==enumDecl== class-side method returning the specification of the enumeration elements, and finally sending the ==initialize== message to the enumeration class we created.

[[[language=smalltalk
FFIEnumeration subclass: #ExampleEnumeration
  instanceVariableNames: ''
  classVariableNames: ''
  package: 'FFITutorial'

ExampleEnumeration class >> enumDecl [
	^ #(
    example0 0
    example1 1
    example2 3
    example3 3
    example4 4
    example5 2147483647
		)
]

ExampleEnumeration initialize.
]]]

Doing this will automatically generate some boilerplate code to manipulate the enumeration.
You will see that the enumeration class gets redefined as follows creating and initializing a class variable for each of its elements.

[[[language=smalltalk
FFIEnumeration subclass: #ExampleEnumeration
  instanceVariableNames: ''
  classVariableNames: 'example0 example1 example2 example3 example4 example5'
  package: 'FFITutorial'
]]]

To use the values of enumerations in our code, it is enough to import it as a pool dictionary, since uFFI enumerations are shared pools.

[[[language=smalltalk
Object subclass: #FFITutorial
  ...
  poolDictionaries: 'ExampleEnumeration'
  ...
]]]

You can use your new enum in two ways:
	- add it to a client class poolDictionaries list (see  #FFIExternalEnumTests for an example), 
		and then just write CCC in your code -- CCC here is an item of your enum
	- send the name of an item to your class --- FFITestEnumeration DDD

The FFIEnumeration implements  (instance and class) some more API methods like:
	#itemAt: retrieves the item having a specific value  --- NBTestEnumeration itemAt: 2
	#includes: checks the existence of a specific item in the enum --- FFITestEnumeration includes: #AAA
"
Class {
	#name : #FFIEnumeration,
	#superclass : #SharedPool,
	#instVars : [
		'value'
	],
	#classInstVars : [
		'representationType'
	],
	#category : #'UnifiedFFI-Objects'
}

{ #category : #converting }
FFIEnumeration class >> asExternalTypeOn: generator [
	^ FFIExternalEnumerationType 
		objectClass: self 
		representationType: self representationType
]

{ #category : #accessing }
FFIEnumeration class >> at: anItem [
	^ self classPool at: anItem ifAbsent: [ nil ]
]

{ #category : #utilities }
FFIEnumeration class >> buildClassVariables [
	self enumerationDictionary keysAndValuesDo: [ :eachName :eachValue | 
		| classBinding enumerationValue |
		
		enumerationValue := (self enumClassForName: eachName) basicNew value: eachValue.
		classBinding := self classPool associationAt: eachName asSymbol ifAbsent: [ nil ].
		classBinding 
			ifNotNil: [ classBinding value: enumerationValue ]
			ifNil: [ 
				self classPool add: (ClassVariable 
					key: eachName asSymbol 
					value: enumerationValue ) ] ]
]

{ #category : #utilities }
FFIEnumeration class >> buildEnumHierarchy [
	"This utility method will create a hierarchy based on enumDecl"
	<script>
	
	self buildClassVariables.
	self rebuildEnumHierarchyAccessors
]

{ #category : #private }
FFIEnumeration class >> defineEnumAccesorFor: enumName [
	"Define accessors as class methods"
	| code |
	
	code := enumName, '
	"This method was automatically generated"
	^ ', enumName.
	
	self class 
		compile: code 
		classified: self generatedEnumProtocolName.
]

{ #category : #utilities }
FFIEnumeration class >> defineEnumHierarchyAccesorFor: enumName [
	"Define accessors as class methods"
	| code |
	
	code := (self enumSelectorName: enumName), '
	"This method was automatically generated"
	^ ', enumName.
	
	self class 
		compile: code 
		classified: self generatedEnumProtocolName
]

{ #category : #utilities }
FFIEnumeration class >> enumClassForName: aName [
	| className |
	
	className := (self name, (self enumNameSuffix: aName)) asSymbol.
	^ self allSubclasses
		detect: [ :each | each name = className ]
		ifNone: [ 
			self 
				subclass: className 
				slots: {  } 
				classVariables: {  } 
				package: self category ]
]

{ #category : #'enum declaration' }
FFIEnumeration class >> enumDecl [
	^ #()
]

{ #category : #utilities }
FFIEnumeration class >> enumNameSuffix: aName [
	^ (aName substrings: '_')
	inject: ''
	into: [ :all :each | all, (each asLowercase capitalized) ]
]

{ #category : #utilities }
FFIEnumeration class >> enumSelectorName: aName [
	^ (self enumNameSuffix: aName) asValidSelector
]

{ #category : #private }
FFIEnumeration class >> enumerationDictionary [
	| decl |
	decl := self enumDecl.
	decl isDictionary
		ifTrue: [ ^ decl ].
	decl := Dictionary newFromPairs: decl.
	^ decl
]

{ #category : #accessing }
FFIEnumeration class >> fromInteger: anIntegerValue [
	| theItem |
	theItem := self itemAt: anIntegerValue.
	theItem ifNil: [ self error: 'Invalid value for ' , self name , ' enumeration' ].
	^ self at: theItem
]

{ #category : #private }
FFIEnumeration class >> generatedEnumProtocolName [
	^ 'accessing enum'
]

{ #category : #testing }
FFIEnumeration class >> includes: aSymbol [
	^ self classPool includesKey: aSymbol
]

{ #category : #'class initialization' }
FFIEnumeration class >> initialize [
	self initializeEnumeration
]

{ #category : #'class initialization' }
FFIEnumeration class >> initializeEnumeration [
	| unsigned |
	
	unsigned := true.
	self enumerationDictionary keysAndValuesDo: [ :eachName :eachValue | 
		| classBinding enumerationValue |
		
		(unsigned and: [ eachValue < 0 ]) 
			ifTrue: [ unsigned := false ].
		
		enumerationValue := self basicNew value: eachValue.
		classBinding := self classPool associationAt: eachName asSymbol ifAbsent: [ nil ].
		classBinding 
			ifNotNil: [ classBinding value: enumerationValue ]
			ifNil: [ 
				self classPool add: (ClassVariable 
					key: eachName asSymbol 
					value: enumerationValue ) ] ].
					
	representationType := unsigned
		ifTrue: [ FFIUInt32 new ]
		ifFalse: [ FFIInt32 new ]
]

{ #category : #accessing }
FFIEnumeration class >> itemAt: aValue [

	^ self 
		itemAt: aValue 
		ifAbsent: [ nil ]
]

{ #category : #accessing }
FFIEnumeration class >> itemAt: aValue ifAbsent: exceptionBlock [

	self classPool associationsDo: [ :assoc | 
		aValue = assoc value value ifTrue: [ 
			^ assoc key ] ].

	^ exceptionBlock value
]

{ #category : #'instance creation' }
FFIEnumeration class >> new [
	^ self shouldNotImplement
]

{ #category : #private }
FFIEnumeration class >> rebuildEnumAccessors [
	self enumerationDictionary keysDo: [ :each | 
		self defineEnumAccesorFor: each ]
]

{ #category : #utilities }
FFIEnumeration class >> rebuildEnumHierarchyAccessors [
	self enumerationDictionary keysDo: [ :each | 
		self defineEnumHierarchyAccesorFor: each ]
]

{ #category : #accessing }
FFIEnumeration class >> representationType [
	^ representationType
]

{ #category : #comparing }
FFIEnumeration >> = anEnumInst [
	^ self class == anEnumInst class 
		and: [ self value = anEnumInst value ]
]

{ #category : #accessing }
FFIEnumeration >> item [
	^ self class itemAt: value
]

{ #category : #printing }
FFIEnumeration >> printOn: stream [
	super printOn: stream.
	stream nextPut: $(;
		 nextPutAll: self item printString;
		 nextPut: $)
]

{ #category : #accessing }
FFIEnumeration >> value [
	^ value
]

{ #category : #accessing }
FFIEnumeration >> value: anObject [
	value := anObject 
]
