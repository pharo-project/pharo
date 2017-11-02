"
FFIExternalEnumerationType reifies the enum declarations in C and offers a nice interface (especially in terms of debug/inspect).
To use it just subclass the FFIExternalEnumeration and add an #enumDecl method to the class side such as:

FFIExternalEnumeration subclass: #FFITestEnumeration
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'NativeBoost-Tests'

FFITestEnumeration class>>enumDecl
	^ {(#AAA -> 1).
	(#BBB -> 2).
	(#CCC -> 3).
	(#DDD -> 2400)} asDictionary 

DO NOT FORGET to call the #initialize method on your class. The rest is automatically done ;)

You can use your new enum in two ways:
	- add it to a client class poolDictionaries list (see  #FFIExternalEnumTests for an example), 
		and then just write CCC in your code -- CCC here is an item of your enum
	- send the name of an item to your class --- FFITestEnumeration DDD

The FFIExternalEnumeration implements  (instance and class) some more API methods like:
	#itemAt: retrieves the item having a specific value  --- NBTestEnumeration itemAt: 2
	#includes: checks the existence of a specific item in the enum --- FFITestEnumeration includes: #AAA
"
Class {
	#name : #FFIExternalEnumeration,
	#superclass : #SharedPool,
	#instVars : [
		'value'
	],
	#classInstVars : [
		'representationType'
	],
	#category : 'UnifiedFFI-Objects'
}

{ #category : #converting }
FFIExternalEnumeration class >> asExternalTypeOn: generator [
	^ FFIExternalEnumerationType 
		objectClass: self 
		representationType: self representationType
]

{ #category : #accessing }
FFIExternalEnumeration class >> at: anItem [
	^ self classPool at: anItem ifAbsent: [ nil ]
]

{ #category : #private }
FFIExternalEnumeration class >> defineEnumAccesorFor: enumName [
	"Define accessors as class methods"
	| code |
	
	code := enumName, '
	"This method was automatically generated"
	^ ', enumName.
	
	self class 
		compile: code 
		classified: self generatedEnumProtocolName.
]

{ #category : #'enum declaration' }
FFIExternalEnumeration class >> enumDecl [
	^ #()
]

{ #category : #private }
FFIExternalEnumeration class >> enumerationDictionary [
	| decl |
	decl := self enumDecl.
	decl isDictionary
		ifTrue: [ ^ decl ].
	decl := Dictionary newFromPairs: decl.
	^ decl
]

{ #category : #accessing }
FFIExternalEnumeration class >> fromInteger: anIntegerValue [
	| theItem |
	theItem := self itemAt: anIntegerValue.
	theItem ifNil: [ self error: 'Invalid value for ' , self name , ' enumeration' ].
	^ self at: theItem
]

{ #category : #private }
FFIExternalEnumeration class >> generatedEnumProtocolName [
	^ 'accessing enum'
]

{ #category : #testing }
FFIExternalEnumeration class >> includes: aSymbol [
	^ self classPool includesKey: aSymbol
]

{ #category : #'class initialization' }
FFIExternalEnumeration class >> initialize [
	self initializeEnumeration
]

{ #category : #'class initialization' }
FFIExternalEnumeration class >> initializeEnumeration [
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
FFIExternalEnumeration class >> itemAt: aValue [
	^ self itemAt: aValue ifAbsent: [ nil ]
]

{ #category : #accessing }
FFIExternalEnumeration class >> itemAt: aValue ifAbsent: exceptionBlock [
	self classPool
		associationsDo: [ :assoc | 
			aValue = assoc value value
				ifTrue: [ ^ assoc key ] ].
	^ exceptionBlock value
]

{ #category : #'instance creation' }
FFIExternalEnumeration class >> new [
	^ self shouldNotImplement
]

{ #category : #private }
FFIExternalEnumeration class >> rebuildEnumAccessors [
	self enumerationDictionary keysDo: [ :each | 
		self defineEnumAccesorFor: each ]
]

{ #category : #accessing }
FFIExternalEnumeration class >> representationType [
	^ representationType
]

{ #category : #comparing }
FFIExternalEnumeration >> = anEnumInst [
	^ self class == anEnumInst class 
		and: [ self value = anEnumInst value ]
]

{ #category : #accessing }
FFIExternalEnumeration >> item [
	^ self class itemAt: value
]

{ #category : #printing }
FFIExternalEnumeration >> printOn: stream [
	super printOn: stream.
	stream nextPut: $(;
		 nextPutAll: self item printString;
		 nextPut: $)
]

{ #category : #accessing }
FFIExternalEnumeration >> value [
	^ value
]

{ #category : #accessing }
FFIExternalEnumeration >> value: anObject [
	value := anObject 
]
