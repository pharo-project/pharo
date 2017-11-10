"
I represent somekind of constant type arrays . 
My main purpose is being used into structures.

For example, a structure like this: 

struct {
	int field[4];
}

can be modeled like this: 

TheStruct class>>initialize
	Int4 := FFITypeArray ofType: #int size: 4 

TheStruct class>>fieldsDesc 
	^ #(
	Int4 field;
	)

Of course this is not the best way to do it (parser needs to be adapted to do this automatically), but is a good and fast way to provide the functionality. 
"
Class {
	#name : #FFITypeArray,
	#superclass : #FFIExternalArray,
	#classInstVars : [
		'type',
		'numberOfElements'
	],
	#category : #UnifiedFFI-Types
}

{ #category : #converting }
FFITypeArray class >> asExternalTypeOn: generator [ 
	^ FFITypeArrayType objectClass: self
]

{ #category : #'instance creation' }
FFITypeArray class >> externalNew [ 
	"Create a new array in the C heap. 
	 This array needs to be disposed (using #free method)... or you can declare it #autoRelease"
	^ self basicNew 
		initializeExternalType: self type size: self numberOfElements;
		yourself
]

{ #category : #'instance creation' }
FFITypeArray class >> fromHandle: aHandle [
	^ self basicNew 
		initializeHandle: aHandle type: self type size: self numberOfElements;
		yourself
]

{ #category : #'instance creation' }
FFITypeArray class >> new [ 
	^ self basicNew 
		initializeType: self type size: self numberOfElements;
		yourself
]

{ #category : #private }
FFITypeArray class >> numberOfElements [
	^ numberOfElements
]

{ #category : #'class factory' }
FFITypeArray class >> ofType: aTypeName size: elements [
	^ FFITypeArray newAnonymousSubclass 
		type: (FFIExternalType resolveType: aTypeName)	
		size: elements
]

{ #category : #private }
FFITypeArray class >> type [ 
	^ type
]

{ #category : #private }
FFITypeArray class >> type: aType 	size: elements [
	type := aType.
	numberOfElements := elements
		
]
