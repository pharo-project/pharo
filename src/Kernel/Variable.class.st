"
I am abstract. Subclasses of me model meta objects for any kind of variables: slots (instance variables), global variables (Class, Global, Pool) and potentialy local variables of methods (temps and args). 
Currently I only unify Slots, LiteralVariables and TemporaryVariable (recently introduced). 

Being a superclass for LiteralVariable I have a restriction on my layout: I can only have a single instance variable representing the variable #name
								DO NOT ADD ANY NEW STATE TO ME.
This restriction is requried because LiteralVariable encodes global references in the method literals and therefore there is an expectation from the VM on particular shape of these objects.

At this step I only pulled a duplicated protocol of properties from my subclasses. But more cleanup is comming:
- breakpoints
- metalinks
- queries
- refactoring commands

Internal Representation and Key Implementation Points.

    Instance Variables
	name:		<Symbol>
"
Class {
	#name : #Variable,
	#superclass : #Object,
	#instVars : [
		'name'
	],
	#classVars : [
		'Properties'
	],
	#category : #'Kernel-Variables'
}

{ #category : #'class initialization' }
Variable class >> initialize [
	Properties := Properties
		ifNil: [ WeakIdentityKeyDictionary new. ]
		ifNotNil: [ (WeakIdentityKeyDictionary newFrom: Properties) rehash]
]

{ #category : #OCVARIABLE }
Variable class >> semanticNodeClass [
	^RBVariableNode
]

{ #category : #comparing }
Variable >> = other [
	"Every subclass that adds state must redefine this method"
	self == other
		ifTrue: [ ^ true ].
	^ (self species == other species) 
			and: [ name = other name ]
]

{ #category : #properties }
Variable >> ensureProperties [
	^ Properties at: self ifAbsentPut: WeakKeyDictionary new
]

{ #category : #properties }
Variable >> hasProperty: aKey [
	"Test if the property aKey is present."
	^self properties 
		ifNil: [false]
		ifNotNil: [:prop | prop includesKey: aKey]
]

{ #category : #comparing }
Variable >> hash [
	"Every subclass that adds state must redefine this method"
	^ self species hash bitXor: self name hash
]

{ #category : #OCVARIABLE }
Variable >> isArg [
	^false
]

{ #category : #OCVARIABLE }
Variable >> isClassVariable [
	^ false
]

{ #category : #OCVARIABLE }
Variable >> isGlobalVariable [
	^ false
]

{ #category : #OCVARIABLE }
Variable >> isInstance [
	^ false
]

{ #category : #OCVARIABLE }
Variable >> isLiteralVariable [
	^false
]

{ #category : #testing }
Variable >> isReferenced [
	^ self subclassResponsibility
]

{ #category : #OCVARIABLE }
Variable >> isRemote [
	^false
]

{ #category : #OCVARIABLE }
Variable >> isSelf [
	^false
]

{ #category : #OCVARIABLE }
Variable >> isSelfOrSuper [
	
	^ false
]

{ #category : #OCVARIABLE }
Variable >> isSpecialVariable [
	^false
]

{ #category : #OCVARIABLE }
Variable >> isSuper [
	^false
]

{ #category : #OCVARIABLE }
Variable >> isTemp [

	^ false
]

{ #category : #OCVARIABLE }
Variable >> isThisContext [
	^false
]

{ #category : #OCVARIABLE }
Variable >> isUndeclared [

	^ false
]

{ #category : #OCVARIABLE }
Variable >> isUninitialized [

	^ false
]

{ #category : #OCVARIABLE }
Variable >> isWritable [
	^ true
]

{ #category : #accessing }
Variable >> name [
	^ name
]

{ #category : #accessing }
Variable >> name: aSymbol [
	name := aSymbol
]

{ #category : #OCVARIABLE }
Variable >> originalVar [
	^ self
]

{ #category : #properties }
Variable >> properties [
	^ Properties at: self ifAbsent: nil
		
]

{ #category : #properties }
Variable >> propertyAt: propName [
	^ self
		propertyAt: propName
		ifAbsent: [ nil ]
		
]

{ #category : #properties }
Variable >> propertyAt: propName ifAbsent: aBlock [
	self properties ifNil: [^aBlock value].
	^ self properties
		at: propName
		ifAbsent: aBlock
		
]

{ #category : #properties }
Variable >> propertyAt: aKey ifAbsentPut: aBlock [
	"Answer the property associated with aKey or, if aKey isn't found store the result of evaluating aBlock as new value."
	
	^ self propertyAt: aKey ifAbsent: [ self propertyAt: aKey put: aBlock value ]
]

{ #category : #properties }
Variable >> propertyAt: propName put: propValue [
	^ self ensureProperties
		at: propName
		put: propValue
]

{ #category : #properties }
Variable >> removePropertiesIfEmpty [
	^ Properties at: self ifPresent: [ :dict |
		dict ifEmpty: [ Properties removeKey: self ] ]
]

{ #category : #properties }
Variable >> removeProperty: propName [
	^ self
		removeProperty: propName
		ifAbsent: [ nil ]
]

{ #category : #properties }
Variable >> removeProperty: propName ifAbsent: aBlock [
	| property |
	self properties ifNil: [^aBlock value].
	property := self properties
		removeKey: propName
		ifAbsent: aBlock.
	self removePropertiesIfEmpty.
	^ property
]

{ #category : #OCVARIABLE }
Variable >> semanticNodeClass [
	^self class semanticNodeClass
]

{ #category : #queries }
Variable >> usingMethods [
	self subclassResponsibility
]

{ #category : #OCVARIABLE }
Variable >> variable [
	"I return the first class variable that I model in the case of globals and ivars"
	^self
]
