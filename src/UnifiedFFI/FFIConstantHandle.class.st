"
I represent a constant HANDLE, as described in  *Windows MSDN>https://msdn.microsoft.com/en-us/library/windows/desktop/ms724457(v=vs.85).aspx*

A ==HANDLE== is a special kind of external object who is accessed through numbers, therefore and ==ExternalAddress== is not appropriate to describe it (since they are constants and external addresses represents disposable spaces from memory).

Is not clear this is necessary outside Windows, but according to documentation they are somekind analogous to unix's File Descriptors (but with some remarkable diferences, as documented *here>http://lackingrhoticity.blogspot.fr/2015/05/passing-fds-handles-between-processes.html*.

Example: 
[[[
User32 class>>initialize 
	HWND := #FFIConstantHandle.

User32 class>>getActiveWindow
	^ self ffiCall: #(HWND GetActiveWindow()) module: 'User32.dll'
]]]
"
Class {
	#name : #FFIConstantHandle,
	#superclass : #FFIExternalReference,
	#category : 'UnifiedFFI-Objects'
}

{ #category : #converting }
FFIConstantHandle class >> asExternalTypeOn: generator [
	^ FFIConstantHandleType objectClass: self
]

{ #category : #'instance creation' }
FFIConstantHandle class >> fromInteger: anIntegerValue [
	anIntegerValue ifNil: [ self error: 'Invalid value for ' , self name , ' contant handle' ].
	^ self new 
		setHandle: anIntegerValue;
		yourself
]

{ #category : #initialization }
FFIConstantHandle >> createDefaultHandle [
	^ 0
]

{ #category : #evaluating }
FFIConstantHandle >> value [
	^ self getHandle
]
