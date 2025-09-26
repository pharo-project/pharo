"
This is a special kind of ByteArray used to be contained on a value holder of a referenced type (see `FFIReferenceValueHolder class>>newType:`. 

This type is needed because in arities > 1 (in external objects, for example), a handle will be ""packed"" (boxed) to be sent, but this is not what we want, we want a simple buffer there.

From Pharo PoV, send a variable `void*` (a reference) and a `void**` is the same, what changes is the semantics to unpack (unbox) the result, and that is relative to what the function defines as API. 

e.g. I can call `somefunction(MyObject **var)` this way: 
```Smalltalk
holder := MyObject newValueHolder.
self callSomeFunction: holder.
""now, result can be a unique instance of MyObject""
var := holder value.
""... or result can be an array of MyObject (size of the array has to be retrieved in  
 some other way)""
varCollection := holder arrayOfSize: 42. 
```

The handle, then, cannot be packed/unpacked other than the first reference, and real 
result meaning depends on what the user wants, and has to be decided later.

THIS CLASS SHOULD NOT BE USED OUTSIDE THE SCOPE IT IS DEFINED.
"
Class {
	#name : 'FFIValueHolderHandle',
	#superclass : 'ByteArray',
	#type : 'bytes',
	#category : 'UnifiedFFI-Objects',
	#package : 'UnifiedFFI',
	#tag : 'Objects'
}

{ #category : 'instance creation' }
FFIValueHolderHandle class >> new [

	^ self new: FFIExternalType pointerSize
]

{ #category : 'private' }
FFIValueHolderHandle >> adoptAddress: anExternalAddress [

	anExternalAddress = 0 ifTrue: [ ^ self atAllPut: 0 ].
	1 to: self size do: [ :index |
		self basicAt: index put: (anExternalAddress at: index) ]
]

{ #category : 'converting' }
FFIValueHolderHandle >> packToArity: aNumber [
]

{ #category : 'converting' }
FFIValueHolderHandle >> unpackFromArity: aNumber [
]
