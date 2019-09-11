"
I am special tag to mark objects that they have particilar kind of children. Kind of children is represented by children type which is class of children objects by default.
For example you can mark class with methods:
	classItem markWithChildrenOf: ClyMethod.

To create my instance manually use:
	ClyItemChildrenTag childrenType: ClyMethod

I cache all my instances. I use class variable ""Types"" to ensure single property instance for each scope class.
	
You can ask browser item to check that object has particular children:
	classItem hasChildrenOf: ClyMethod

Internal Representation and Key Implementation Points.

    Instance Variables
	childrenType:		<Class>
"
Class {
	#name : #ClyItemChildrenTag,
	#superclass : #ClyProperty,
	#instVars : [
		'childrenType'
	],
	#classVars : [
		'Types'
	],
	#category : #'Calypso-NavigationModel-Tag'
}

{ #category : #'instance creation' }
ClyItemChildrenTag class >> childrenType: itemTypeClass [
	^self types at: itemTypeClass ifAbsentPut: [ 
		self new childrenType: itemTypeClass]
]

{ #category : #cache }
ClyItemChildrenTag class >> types [
	^Types ifNil: [ Types := IdentityDictionary new ]
]

{ #category : #accessing }
ClyItemChildrenTag >> childrenType [
	^ childrenType
]

{ #category : #accessing }
ClyItemChildrenTag >> childrenType: aClass [
	childrenType := aClass
]

{ #category : #printing }
ClyItemChildrenTag >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $(.
	childrenType printOn: aStream.
	aStream nextPut: $).
]
