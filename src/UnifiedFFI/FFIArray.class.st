"
I represent a C array with fixed size.

## Creating instances

To create an instance of myself, use the ==newType:size:== or the  ==externalNewType:size:== instance creation methods. The former will allocate an array in the Pharo heap, while the latter one will allocate the array in the C heap.

""In the pharo heap""
array := FFIArray newType: #char size: 10.
""In the C heap""
array := FFIArray externalNewType: #char size: 10.

Arrays allocated in the C heap are not moved and their memory is not released automatically.
It is the developer's responsibility to free it.
On the other hand, Arrays allocated in the Pharo heap can be moved by the garbage collector, so they should be pinned in memory before being safely used in FFI calls.
Also, arrays in the Pharo heap are managed by Pharo's garbage collector, and will be collected if no other Pharo objects reference it. Be careful, an Array in the Pharo heap referenced from the C heap will still be garbage collected making the pointer in the C heap a dangling pointer.

## Accessing my instances

You can access my elements using the #at: and #at:put: methods, and ask my size using the #size method, using 1-based indexes like in Pharo.

array at: 1 ""for the first element"".
array at: n ""for the nth element"".

If the array is allocated in the Pharo heap, array accesses will be bound checked and throw an exception in case of out-of-bounds access.
Otherwise, if the array is allocated in the C heap, array accesses may run into buffer over/underflows.

## Reusable Array types

I can be used to create array types with the ==newArrayType:size:== method, which yields a new array type knowing the element types and the array size. An array type created this way can be then be allocated using the new or externalNew messages.

char128Type := FFIArray newArrayType: #char size: 128.

""In Pharo heap""
newArrayOf128Chars := char128Type new.

""In C heap""
newArrayOf128Chars := char128Type externalNew.

My main purpose is to be used in structures, for example:

struct {
	int field[4];
}

can be modeled like this: 

TheStruct class>>initialize
	Int4 := FFIArray newArrayType: #int size: 4.

TheStruct class>>fieldsDesc 
	^ #(
	Int4 field;
	)

Of course this is not the best way to do it (parser needs to be adapted to do this automatically), but is a good and fast way to provide the functionality. 
"
Class {
	#name : 'FFIArray',
	#superclass : 'FFIExternalArray',
	#classVars : [
		'DefinedTypes'
	],
	#classInstVars : [
		'type',
		'numberOfElements'
	],
	#category : 'UnifiedFFI-Objects',
	#package : 'UnifiedFFI',
	#tag : 'Objects'
}

{ #category : 'converting' }
FFIArray class >> asExternalTypeOn: generator [
	^ FFITypeArrayType objectClass: self
]

{ #category : 'instance creation' }
FFIArray class >> externalNew [
	"Create a new array in the C heap.
	 This array needs to be disposed (using #free method)... or you can declare it #autoRelease"
	^ self basicNew
		initializeExternalType: self type size: self numberOfElements;
		yourself
]

{ #category : 'private' }
FFIArray class >> findOrCreateType: aTypeName size: aSize [

	DefinedTypes ifNil: [ DefinedTypes := SmallDictionary new ].
	^ (DefinedTypes 
		at: aTypeName ifAbsentPut: [ SmallDictionary new ])
		at: aSize ifAbsentPut: [ self newArrayTypeOf: aTypeName size: aSize ]
]

{ #category : 'instance creation' }
FFIArray class >> fromHandle: aHandle [
	^ self basicNew
		initializeHandle: aHandle type: self type size: self numberOfElements;
		yourself
]

{ #category : 'instance creation' }
FFIArray class >> new [
	^ self basicNew
		initializeType: self type size: self numberOfElements;
		yourself
]

{ #category : 'private' }
FFIArray class >> newArrayTypeOf: aTypeName size: aSize [

	^ FFIArray newAnonymousSubclass
		type: (FFIExternalType resolveType: aTypeName)
		size: aSize
]

{ #category : 'private' }
FFIArray class >> numberOfElements [
	^ numberOfElements
]

{ #category : 'class factory' }
FFIArray class >> ofType: aTypeName size: aSize [

	^ self 
		findOrCreateType: aTypeName 
		size: aSize
]

{ #category : 'private' }
FFIArray class >> type [
	^ type
]

{ #category : 'private' }
FFIArray class >> type: aType 	size: elements [
	type := aType.
	numberOfElements := elements
]
