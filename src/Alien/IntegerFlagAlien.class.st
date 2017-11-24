"
A class to wrap the inIoProcessEvents flag in the WIndows VM.
"
Class {
	#name : #IntegerFlagAlien,
	#superclass : #Alien,
	#type : #bytes,
	#category : #'Alien-Win32'
}

{ #category : #'instance creation' }
IntegerFlagAlien class >> forInIoProcessEventsFlag [
	^self
		atAddress: self primInIOProcessEventsFlagAddress
		dataSize: 4
]

{ #category : #'primitives-Windows-VM-specific' }
IntegerFlagAlien class >> primInIOProcessEventsFlagAddress [ "^<Integer>"
	"Answer the address of the Windows-VM-specific int inIoProcessEvents flag.
	 See platforms/win32/vm/sqWin32Window.c
	 and platforms/Cross/plugins/IA32ABI/ia32abicc.c."
	<primitive: 'primInIOProcessEventsFlagAddress' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #accessing }
IntegerFlagAlien >> decrement [
	| value |
	(value := self value) > 0 ifTrue:
		[self value: value - 1]
]

{ #category : #accessing }
IntegerFlagAlien >> increment [
	self value: self value + 1
]

{ #category : #accessing }
IntegerFlagAlien >> value [
	^self signedLongAt: 1
]

{ #category : #accessing }
IntegerFlagAlien >> value: anInteger [
	self signedLongAt: 1 put: anInteger
]
