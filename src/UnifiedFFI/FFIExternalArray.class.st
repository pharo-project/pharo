"
I am class which provides a convenient interface to work with arrays which elements are values of some external (C) type.
This class uses double dispatch to solve #at: and #at:put: methods, if you need to achieve speed, is recommendable to subclass me and override this methods to avoid the indirection.
"
Class {
	#name : #FFIExternalArray,
	#superclass : #ArrayedCollection,
	#instVars : [
		'handle',
		'type',
		'size'
	],
	#category : #'UnifiedFFI-Deprecated'
}

{ #category : #converting }
FFIExternalArray class >> asExternalTypeOn: generator [ 
	^ FFIExternalArrayType objectClass: self
]

{ #category : #'instance creation' }
FFIExternalArray class >> externalNewType: aType size: aNumber [
	"Create a new array in the C heap. 
	 This array needs to be disposed (using #free method)"
	^ self basicNew 
		initializeExternalType: (self resolveType: aType) size: aNumber;
		yourself
]

{ #category : #'instance creation' }
FFIExternalArray class >> fromHandle: aHandle type: aType [
	"Instantiating a handle as an array.
	 USE THIS WITH EXTREMELY CAUTION: You could have an overflow very easily, because you are 
	 not expliciting the size. 
	 I would prefer if you use #fromHandle:type:size: instead. "
	^ self fromHandle: aHandle type: aType size: nil
]

{ #category : #'instance creation' }
FFIExternalArray class >> fromHandle: aHandle type: aType size: aNumber [
	"aHandle is the address to an array already existing. 
	 For example, C: 
	 	
		byte[]	 aHandle;
			
	 which means is a direct reference and array starts in same address as aHandle"
	^ self basicNew 
		initializeHandle: aHandle type: (self resolveType: aType) size: aNumber;
		yourself
]

{ #category : #'instance creation' }
FFIExternalArray class >> fromPointer: aHandle type: aType [ 
	"Instantiating a pointer as an array is usefull, but then you do not have the size. 
	 USE THIS WITH EXTREMELY CAUTION: You could have an overflow very easily. 
	 I would prefer if you use #fromPointer:type:size: instead. "
	^ self fromPointer: aHandle type: aType size: nil
]

{ #category : #'instance creation' }
FFIExternalArray class >> fromPointer: aHandle type: aType size: aNumber [
	"Creates an array of type aType and size aNumber.
	 aHandle is A REFERENCE an array already existing. 
	 For example, C: 
	 	
		byte[]	 array;
		void * aHandle = &array;
			
	 Which means first element of array will be found at ==aHandle pointerAt: 1=="
	^ self fromHandle: (aHandle pointerAt: 1) type: aType size: aNumber
]

{ #category : #'instance creation' }
FFIExternalArray class >> new [ 
	self error: 'should not create external arrays like this'
]

{ #category : #'instance creation' }
FFIExternalArray class >> newType: aType size: aNumber [
	"Create a new array using internal memory."
	^ self basicNew 
		initializeType: (self resolveType: aType) size: aNumber;
		yourself
]

{ #category : #private }
FFIExternalArray class >> resolveType: aType [
	 ^ FFICallout new 
		requestor: self;
		resolveType: aType.
]

{ #category : #converting }
FFIExternalArray >> asArray [ 
	| array |
	
	array := Array new: self size.
	1 to: self size do: [ :index | array at: index put: (self at: index) ].
	
	^ array 
]

{ #category : #accessing }
FFIExternalArray >> at: index [ 
	^ type 
		handle: handle
		at: ((index - 1) * type typeSize) + 1
]

{ #category : #accessing }
FFIExternalArray >> at: index put: anObject [ 
	^ type 
		handle: handle
		at: ((index - 1) * type typeSize) + 1
		put: anObject
]

{ #category : #'external resource management' }
FFIExternalArray >> autoRelease [
	(handle isNil or: [ handle isExternalAddress not ]) ifFalse: [ ^ self ].
	handle autoRelease
]

{ #category : #finalization }
FFIExternalArray >> free [
	handle ifNil: [ ^ self ]. 
	handle isExternalAddress 
		ifTrue: [ handle free ]
		ifFalse: [ handle := nil ]
]

{ #category : #converting }
FFIExternalArray >> gcpointer [
	"Answers a pointer to the this array. 
	 This is useful when translating an array pointer to a function (FFI requires to be passed as a
	 pointer, otherwise will be interpreted as a direct array and it will not work fine)
	
	 For an example, see FFICallback class>>#exampleCqsort
	
	 WARNING: Only valid for external address (you cannot have a pointer to an image side object)"
	self deprecated: 'Use "pointer autoRelease" instead.' on: '2016-01-22' in: #Pharo5.  		
	self getHandle isExternalAddress ifFalse: [ 
		self error: 'Arrays need to be moved to external memory space before passing them as pointers.' ].
	^ self getHandle gcpointer	
]

{ #category : #accessing }
FFIExternalArray >> getHandle [
	self flag: #pharoTodo. "I do not like the name 'getHandle' but I'm looking to unify the 
	API with FFI" 
	^ handle
]

{ #category : #initialization }
FFIExternalArray >> initializeExternalType: aType size: aNumber [
	self 
		initializeType: aType 
		size: aNumber 
		handleProducer: [ :totalSize | ExternalAddress allocate: (type typeSize * size) ]
]

{ #category : #initialization }
FFIExternalArray >> initializeHandle: aHandle type: aType size: aNumber [
	self 
		initializeType: aType 
		size: aNumber 
		handleProducer: [ :totalSize | aHandle ]
]

{ #category : #initialization }
FFIExternalArray >> initializeType: aType size: aNumber [
	self 
		initializeType: aType 
		size: aNumber 
		handleProducer: [ :totalSize | ByteArray new: totalSize ]

]

{ #category : #initialization }
FFIExternalArray >> initializeType: aType size: aNumber handleProducer: aBlock [ 
	type := aType.
	size := aNumber.
	handle := aBlock value: (size 
		ifNotNil: [ size * type typeSize ]
		ifNil: [ 0 ]).
	handle isNull ifTrue: [ self error: 'External allocation failed' ].
	self initialize
]

{ #category : #converting }
FFIExternalArray >> pointer [
	"Answers a pointer to this array. 
	 This is useful when translating an array pointer to a function (FFI requires to be passed as a
	 pointer, otherwise will be interpreted as a direct array and it will not work fine)
	
	 For an example, see FFICallback class>>#exampleCqsort
	
	 WARNING: Only valid for external address (you cannot have a pointer to an image side object)"
		
	self getHandle isExternalAddress ifFalse: [ 
		self error: 'Arrays need to be moved to external memory space before passing them as pointers.' ].
	^ self getHandle pointer	
]

{ #category : #converting }
FFIExternalArray >> pointerAutoRelease [
	"Same as #pointer (see its comment for detals), but contents are garbage collected automatically"
	^ self pointer autoRelease
]

{ #category : #initialization }
FFIExternalArray >> size [
	^ size
]

{ #category : #accessing }
FFIExternalArray >> type [
	^ type
]

{ #category : #accessing }
FFIExternalArray >> typeAlignment [ 
	^ self type typeAlignment
]

{ #category : #accessing }
FFIExternalArray >> typeSize [ 
	^ self type typeSize
]
