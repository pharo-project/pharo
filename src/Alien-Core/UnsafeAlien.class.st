"
Instances of UnsafeAlien represent the addresses of heap-resident non-pointer Smalltalk objects as actual parameters in FFI call-outs.  An UnsafeAlien on (e.g.) a ByteString used as a parameter in an FFI call causes the FFI machinery to pass the address of the first byte in the ByteString.  THIS IS UNSAFE!  It is unsafe because
a) the garbage collector can potentially move the ByteString (or any other object) during the call, because the call may call-back, invoking the garbage collector,
b) if external code retains the address for longer than the duration of the call and dereferences it in a subsequent call the object may have moved in the mean time,
c) the address of the object is passed without any other potentially necessary conversions such as null-termination
d) the hundred other problems this benighted author hasn't thought of.
Hence UnsafeAlien is to be used carefully by clients that know that the usage is safe.
You have been warned ;)

Create instances via
	UnsafeAlien forPointerTo: 'You are on your own!', (ByteString with: (Character value: 0))
"
Class {
	#name : #UnsafeAlien,
	#superclass : #Object,
	#instVars : [
		'nonPointerObject'
	],
	#category : #'Alien-Core'
}

{ #category : #'class initialization' }
UnsafeAlien class >> ensureInSpecialObjectsArray [
	"UnsafeAlien must be in the specialObjectsArray to enable the Newsqueak FFI.
	 Because of the bootstrap the specialObjectsArray may already have been
	 partially initialized with the callback selector and to be large enough (as part of
	 loading immutability), but it will be missing UnsafeAlien until UnsafeAlien is loaded.
	 So check if the specialObjectsArray is of the expected size before recreating
	 and slam UnsafeAlien in if the specialObjectsArray is already large enough."

	| index env|
	env := self environment.
	self ~~ UnsafeAlien ifTrue: [^self].

	index := 55.
	((env includesKey: #VMObjectIndices)
	 and: [((env at: #VMObjectIndices) classPool at: #ClassUnsafeAlien ifAbsent: []) ~= (index - 1)]) ifTrue:
		[self error: 'index probably wrong'].

	Smalltalk specialObjectsArray size < index ifTrue:
		[Smalltalk recreateSpecialObjectsArray].
	
	(Smalltalk specialObjectsArray size < index
	 or: [(Smalltalk specialObjectsArray at: index) ~~ nil
		and: [(Smalltalk specialObjectsArray at: index) ~~ self]]) ifTrue:
		[self error: 'SystemDictionary>>recreateSpecialObjectsArray appears incompatible'].

	(Smalltalk specialObjectsArray at: index) ifNil:
		[Smalltalk specialObjectsArray at: index put: self]
]

{ #category : #examples }
UnsafeAlien class >> exampleSprintf [
	"Call the libc sprintf function."
	"UnsafeAlien exampleSprintf"
	| r s |
	(Alien lookup: 'sprintf' inLibrary: Alien libcName)
		primFFICallResult: (r := Alien new: 4)
		withArguments: {self forPointerTo: (s := ByteString new: 1024).
						 self forPointerTo: 'Hello World %d %x !!', (String with: Character lf).
						 123.
						 48879}.
	^s copyFrom: 1 to: (r signedLongAt: 1) - 1 "trim the null byte"
]

{ #category : #'instance creation' }
UnsafeAlien class >> forPointerTo: anObject [
	(anObject class instSpec between: 6 and: 8) ifFalse:
		[^self error: 'only use UnsafeAlien on instances of isBits classes (other than CompiledMethod)!'].
	^self new nonPointerObject: anObject
]

{ #category : #'class initialization' }
UnsafeAlien class >> initialize [
	"UnsafeAlien initialize"
	self ensureInSpecialObjectsArray
]

{ #category : #accessing }
UnsafeAlien >> object [
	^nonPointerObject
]
