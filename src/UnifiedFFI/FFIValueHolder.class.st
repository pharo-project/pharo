"
A value holder is a container to values that will be stored here by C functions.  
In C, whenever you have a function, for example with the form: `function(int *var)`, the function receives not the variable, but a reference to it (the position in memory, or `ExternalAdress` in our jargon).  
This makes that for our intends, if we want to call a function and get the value of it later, we need to do things like: 

```smalltalk
varHolder := ByteArray new: 8. ""The size of a pointer in 64bit""
self function(var).
var := varHolder signedLongAt: 1.
```

and the thing becomes more and more complicated when we need to pass structures, arrays, objects, etc.  
To easy this we first (some years ago, in fact) introduced the notion of `buffers` that can be accessed through the atomic/basic C types of uFFI, e.g. : `FFIInt64 newBuffer`, but of course as solution this is just partial and it lets still a lot of work for the user.  
The new mechanism is very simple to use : you just ask for a value holder to the type you want to pass, and then ask for the resulting value to it.  
Last example will look like this:  

```smalltalk
varHolder := FFIInt64 newValueHolder.
self function(varHolder).
var := varHolder value.
```

While this does not look like a huge change, see how the same change will affect an opaque object call 9I took the example from a recently added library to our catalog, [pharo-resvg](https://github.com/estebanlm/pharo-resvg)):  

```smalltalk
tree := ResvgRenderTree basicNew setHandle: (ByteArray new: 8).
err := resvg 
	parse_tree_from_data: source 
	len: source size
	opt: options
	tree: tree.
tree setHandle: (tree getHandle pointerAt: 1).
```
As you see, the accesses there are ""hacky"" at best. And it requires a lot of knowledge on how the things work inside, deep in uFFI.  
The new mechanism allow us to write the same as this:  

```smalltalk
treeHolder := ResvgRenderTree newValueHolder.
err := resvg 
	parse_tree_from_data: source 
	len: source size
	opt: options
	tree: treeHolder.
tree := treeHolder value.
```

And the same will apply to *any* structure, opaque object or basic type we want to pass as reference to C.
"
Class {
	#name : 'FFIValueHolder',
	#superclass : 'Object',
	#instVars : [
		'handle',
		'type'
	],
	#category : 'UnifiedFFI-Objects',
	#package : 'UnifiedFFI',
	#tag : 'Objects'
}

{ #category : 'instance creation' }
FFIValueHolder class >> newType: aType [

	^ self 
		newType: aType 
		handle: aType newBuffer
]

{ #category : 'instance creation' }
FFIValueHolder class >> newType: aTypeClass handle: aHandle [

	^ self basicNew
	initializeType: aTypeClass handle: aHandle;
	yourself
]

{ #category : 'private' }
FFIValueHolder >> adoptAddress: anExternalAddress [

	handle := anExternalAddress getHandle
]

{ #category : 'accessing' }
FFIValueHolder >> arrayOf: aTypeOrClass size: aNumber [

	^ self 
		basicArrayOfType: (FFIExternalType resolveType: aTypeOrClass)
		size: aNumber
]

{ #category : 'accessing' }
FFIValueHolder >> arrayOfSize: aNumber [

	^ self 
		basicArrayOfType: self type
		size: aNumber
]

{ #category : 'private' }
FFIValueHolder >> basicArrayOfType: aType size: aNumber [
	| next |

	next := self firstHandle.
	^ Array streamContents: [ :stream |
		0 to: aNumber - 1 do: [ :index |
			| value |
			value := aType handle: next at: 1.
			stream nextPut: value.
			next := next + aType externalTypeSize ] ]
]

{ #category : 'private' }
FFIValueHolder >> firstHandle [

	^ self getHandle pointerAt: 1
]

{ #category : 'accessing' }
FFIValueHolder >> getHandle [
	"for compatibility"

	^ handle
]

{ #category : 'accessing' }
FFIValueHolder >> handle [

	^ handle
]

{ #category : 'initialization' }
FFIValueHolder >> initializeType: aType handle: aHandle [

	type := aType.
	handle := aHandle.
	self initialize
]

{ #category : 'converting' }
FFIValueHolder >> packToArity: aNumber [
	"holders do not pack arity"

	^ self
]

{ #category : 'private' }
FFIValueHolder >> pointer [
	"used for boxing/unboxing the typed pointers e.g. int* with take a pointer"
	
	^ self getHandle
]

{ #category : 'accessing' }
FFIValueHolder >> type [

	^ type
]

{ #category : 'converting' }
FFIValueHolder >> unpackFromArity: aNumber [
	"holders do not pack arity"

	^ self
]

{ #category : 'accessing' }
FFIValueHolder >> value [

	^ self type 
		handle: self getHandle 
		at: 1
]

{ #category : 'accessing' }
FFIValueHolder >> value: anObject [

	^ self type 
		handle: self getHandle 
		at: 1
		put: anObject
]
