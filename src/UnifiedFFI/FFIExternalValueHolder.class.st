"
I am a generic meta class for creating a subclasses, which instances will hold a single value of specified C type.
To create a new class for some concrete C type you need to declare an anonnymous class and indicate the type you want to use:

[ [ [ 
floatHolderClass := FFIExternalValueHolder ofType: 'float'
 ] ] ]

Then you can use  given class(es) as a value holders for type you specified:

[ [ [ 
 float := floatHolderClass new.

float value:  1.5
float value
 ] ] ]

By combining this with class/pool variables we have a convenient way of defining a values, which are passed by pointer to a function.

For instance, imagine that we need to create a binding to a function:

[ [ [ 
void getFoo( SomeType * value) 
 ] ] ]

which is not an unusual situation, when C function using pointer arguments for storing it's output there.

And this is what ==FFIExternalValueHolder== is done for. To define a binding to this function you can declare a subclass named SomeType
 
and then use it in function signature:

[ [ [ 
getFoo: value
  ^ self ffiCall: #(void getFoo ( SomeType *  value ))
] ] ]

and call it like: 

[ [ [ 
var := SomeType new.
self getFoo: var.

var value  ""will read the value"" 
]]]
"
Class {
	#name : #FFIExternalValueHolder,
	#superclass : #Object,
	#instVars : [
		'data'
	],
	#classInstVars : [
		'type'
	],
	#category : #'UnifiedFFI-Objects'
}

{ #category : #converting }
FFIExternalValueHolder class >> asExternalTypeOn: generator [
	^ FFIExternalValueHolderType 
		objectClass: self 
		representationType: self type
]

{ #category : #'class factory' }
FFIExternalValueHolder class >> ofType: aTypeName [
	^ FFIExternalValueHolder newAnonymousSubclass 
		type: (FFIExternalType resolveType: aTypeName)
]

{ #category : #initialization }
FFIExternalValueHolder class >> reset [
	type := nil
]

{ #category : #private }
FFIExternalValueHolder class >> type [
	^ type ifNil: [ type := FFIExternalType resolveType: self typeDecl ]
]

{ #category : #private }
FFIExternalValueHolder class >> type: aType [ 
	type := aType
]

{ #category : #accessing }
FFIExternalValueHolder class >> typeDecl [
	"Define the type for this value holder. 
	 It can be anything that FFIExternalType class>>resolveType: can resolve. 
	 If you need something different, you can override #representationType"
	^ self subclassResponsibility
]

{ #category : #initialization }
FFIExternalValueHolder >> initialize [
	data := ByteArray new: self type typeSize.
	super initialize
]

{ #category : #accessing }
FFIExternalValueHolder >> type [ 
	^ self class type
]

{ #category : #accessing }
FFIExternalValueHolder >> value [ 
	^ self type 
		handle: data 
		at: 1
		
]

{ #category : #accessing }
FFIExternalValueHolder >> value: anObject [ 
	self type 
		handle: data 
		at: 1
		put: anObject
]
