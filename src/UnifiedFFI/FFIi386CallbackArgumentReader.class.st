"
I am the subclass responsible of handling i386 parameter passing.
All the parameters in i386 are passed in the stack.
The integer types are promoted to Int32 or UInt32 depending if they are signed or unsigned.
The structs are always passed in the stack.

I do not modify the behavior of my superclass.
"
Class {
	#name : #FFIi386CallbackArgumentReader,
	#superclass : #FFICallbackArgumentReader,
	#category : #'UnifiedFFI-Callbacks'
}

{ #category : #types }
FFIi386CallbackArgumentReader >> stackIntegerType [
	
	^ FFIInt32 new
]

{ #category : #types }
FFIi386CallbackArgumentReader >> stackUnsignedIntegerType [
	
	^ FFIUInt32 new
]
