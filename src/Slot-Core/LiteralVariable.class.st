"
I am abstract. Subclasses of me model meta objects for global variables (Class, Global, Pool), called ""Literal Variables"" in Smalltalk.

I am just a subclass of Association, modeling the binding of the global in either the Smalltalk globals, Undeclared or classPools.


When chaning emit* methods, do not forget to recompile exisiting code:

aGlobal usingMethods do: #recompile 

***NOTE***
When moving binding from Undeclared, we change the class of that binding to either ClassVariable or GlobalVariable.

==> when we use Global subclasses, we will either need to restrict adding variables or add a slow path where we create a new binding and update all users. But this can be done later.
"
Class {
	#name : #LiteralVariable,
	#superclass : #Association,
	#classVars : [
		'Properties'
	],
	#category : #'Slot-Core-Variables'
}

{ #category : #validating }
LiteralVariable class >> checkValidName: aString [ 
	"aString first canBeGlobalVarInitial
		ifFalse: [InvalidGlobalName signal: 'Class name does not start with a valid Global Var Initial' for: aString]"
]

{ #category : #'class initialization' }
LiteralVariable class >> initialize [
	Properties
		ifNil: [ self initializeProperties ]
		ifNotNil: [ | newDict | 
			newDict := WeakIdentityKeyDictionary newFrom: Properties.
			newDict rehash.
			Properties := newDict ]
]

{ #category : #'class initialization' }
LiteralVariable class >> initializeProperties [
	Properties := WeakIdentityKeyDictionary new.
]

{ #category : #'instance creation' }
LiteralVariable class >> named: aSymbol [ 
	self checkValidName: aSymbol.
	^ self new
		name: aSymbol;
		yourself
]

{ #category : #converting }
LiteralVariable >> asClassVariable [	
	^self
]

{ #category : #queries }
LiteralVariable >> definingClass [
	"The class defining the variable. For Globals, return nil"
	^Smalltalk globals allClasses detect: [ :class | class hasClassVariable: self ] ifNone: [ nil ]
]

{ #category : #printing }
LiteralVariable >> definitionString [
	^String streamContents: [:aStream | aStream << '#' << self name << ' => '; << self class name].
]

{ #category : #'code generation' }
LiteralVariable >> emitStore: aMethodBuilder [
	| tempName |
	tempName := Object new.
	aMethodBuilder
		addTemp: tempName;
		storeTemp: tempName;
		popTop;
		pushLiteralVariable: #global -> self;
		pushTemp: tempName;
		send: #write:
]

{ #category : #'code generation' }
LiteralVariable >> emitValue: aMethodBuilder [
	aMethodBuilder
		pushLiteralVariable: #global -> self;
		send: #read
]

{ #category : #properties }
LiteralVariable >> ensureProperties [
	^ Properties at: self ifAbsentPut: WeakKeyDictionary new
]

{ #category : #properties }
LiteralVariable >> hasProperty: aKey [
	"Test if the property aKey is present."
	^self properties 
		ifNil: [false]
		ifNotNil: [:prop | prop includesKey: aKey]
]

{ #category : #testing }
LiteralVariable >> isClassVariable [
	^false
]

{ #category : #testing }
LiteralVariable >> isGlobal [
	^true
]

{ #category : #testing }
LiteralVariable >> isGlobalVariable [
	^false
]

{ #category : #testing }
LiteralVariable >> isReadIn: aCompiledCode [
	^aCompiledCode readsRef: self
]

{ #category : #testing }
LiteralVariable >> isSelfEvaluating [
	^ self key isSelfEvaluating and: [self value isSelfEvaluating]
]

{ #category : #testing }
LiteralVariable >> isSpecial [
	"I am more than just a backward compatible Global"
	^true
]

{ #category : #testing }
LiteralVariable >> isWrittenIn: aCompiledCode [
	^aCompiledCode writesRef: self
]

{ #category : #accessing }
LiteralVariable >> name: aString [ 
	self key: aString asSymbol
]

{ #category : #properties }
LiteralVariable >> properties [
	^ Properties at: self ifAbsent: nil
		
]

{ #category : #properties }
LiteralVariable >> propertyAt: propName [
	^ self
		propertyAt: propName
		ifAbsent: [ nil ]
		
]

{ #category : #properties }
LiteralVariable >> propertyAt: propName ifAbsent: aBlock [
	self properties ifNil: [^aBlock value].
	^ self properties
		at: propName
		ifAbsent: aBlock
		
]

{ #category : #properties }
LiteralVariable >> propertyAt: aKey ifAbsentPut: aBlock [
	"Answer the property associated with aKey or, if aKey isn't found store the result of evaluating aBlock as new value."
	
	^ self propertyAt: aKey ifAbsent: [ self propertyAt: aKey put: aBlock value ]
]

{ #category : #properties }
LiteralVariable >> propertyAt: propName put: propValue [
	^ self ensureProperties
		at: propName
		put: propValue
]

{ #category : #'meta-object-protocol' }
LiteralVariable >> read [
	^self value
]

{ #category : #properties }
LiteralVariable >> removePropertiesIfEmpty [
	^ Properties at: self ifPresent: [ :dict |
		dict ifEmpty: [ Properties removeKey: self ] ]
]

{ #category : #properties }
LiteralVariable >> removeProperty: propName [
	^ self
		removeProperty: propName
		ifAbsent: [ nil ]
]

{ #category : #properties }
LiteralVariable >> removeProperty: propName ifAbsent: aBlock [
	| property |
	self properties ifNil: [^aBlock value].
	property := self properties
		removeKey: propName
		ifAbsent: aBlock.
	self removePropertiesIfEmpty.
	^ property
]

{ #category : #queries }
LiteralVariable >> usingMethods [
	^SystemNavigation new allReferencesTo: self
]

{ #category : #'meta-object-protocol' }
LiteralVariable >> write: anObject [
	self value: anObject.
	^anObject
]
