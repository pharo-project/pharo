"
Copyright 2008 Cadence Design Systems, Inc.
   
   Licensed under the Apache License, Version 2.0 (the ''License''); you may not use this file except in compliance with the License.  You may obtain a copy of the License at  http://www.apache.org/licenses/LICENSE-2.0

Instances of Alien represent actual parameters, return results and function pointers in FFI call-outs and call-backs and provide handles on external data.  See NewsqueakIA32ABIPlugin for the VM code that actually implements call-outs and call-backs.

See the class-side examples category for some simple example workspaces.

Aliens represent ABI (C language) data.  They can hold data directly in their bytes or indirectly by pointing to data on the C heap.  Alien instances are at least 5 bytes in length. The first 4 bytes of an Alien hold the size, as a signed integer, of the datum the instance is a proxy for.  If the size is positive then the Alien is ""direct"" and the actual datum resides in the object itself, starting at the 5th byte.  If the size is negative then the proxy is ""indirect"", is at least 8 bytes in length and the second 4 bytes hold the address of the datum, which is assumed to be on the C heap.  Any attempt to access data beyond the size will fail.  If the size is zero then the Alien is a pointer, the second 4 bytes hold a pointer, as for ""indirect"" Aliens, and accessing primitives indirect through the pointer to access data, but no bounds checking is performed.

When Aliens are used as parameters in FFI calls then all are ""passed by value"", so that e.g. a 4 byte direct alien will have its 4 bytes of data passed, and a 12-byte indirect alien will have the 12 bytes its address references passed.  Pointer aliens will have their 4 byte pointer passed.  So indirect and pointer aliens are equivalent for accessing data but different when passed as parameters, indirect Aliens passing the data and pointer Aliens passing the pointer.

Class Variables:
GCMallocedAliens <AlienWeakTable of <Alien -> Integer>> - weak collection of malloced aliens, used to free malloced memory of Aliens allocated with newGC:
LoadedLibraries <Dictionary of <String -> Alien>> - library name to library handle map

Class instance variables
lifoCallbackSemaphore <Semaphore> - The lifoCallbackSemaphore queues callbacks which cannot return because they are not the most recent callback.  Callback return must be last-in-first-out (LIFO) to avoid scrambling the C stack.  See Alien class>>invokeCallbackContext:.
"
Class {
	#name : #Alien,
	#superclass : #ByteArray,
	#type : #bytes,
	#classVars : [
		'GCMallocedAliens',
		'LoadedLibraries',
		'ProcessIoEvents',
		'WordSize'
	],
	#classInstVars : [
		'lifoCallbackSemaphore'
	],
	#category : #'Alien-Core'
}

{ #category : #'primitives-memory management' }
Alien class >> Ccalloc: byteSize [ "<Integer> ^<Integer>"
	<primitive: 'primCalloc' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-memory management' }
Alien class >> Cmalloc: byteSize [ "<Integer> ^<Integer>"
	<primitive: 'primMalloc' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'instance creation' }
Alien class >> atAddress: pointer [ "<Integer>" "^<Alien>"
	"N.B.  This method assumes one is creating a pointer to already initialized data, hence initiaize is not sent.
	(Perhaps some alternative, eg. initializePointer, should be sent?"
	^(self basicNew: WordSize * 2)
		sizeFieldPut: self dataSize negated;
		addressFieldPut: pointer;
		yourself
]

{ #category : #'instance creation' }
Alien class >> atAddress: address "<Integer>" dataSize: dataByteSize [ "<Integer>" "^<Alien>"
	"N.B.  This method assumes one is creating a pointer to already initialized data, hence initiaize is not sent.
	(Perhaps some alternative, eg. initializePointer, should be sent?"
	^(self basicNew: WordSize * 2)
		sizeFieldPut: dataByteSize asInteger abs negated;
		addressFieldPut: address;
		yourself
]

{ #category : #'instance creation' }
Alien class >> dataSize [ "^<Integer>"
	"Answer the number of bytes required to represent an instance of the receiver.
	This excludes the size word's own bytes and is just the size of the data."
	^self subclassResponsibility
]

{ #category : #'class initialization' }
Alien class >> ensureInSpecialObjectsArray [
	"Alien must be in the specialObjectsArray to enable the Newsqueak FFI.  Because of
	 the bootstrap the specialObjectsArray may already have been partially initialized with
	 the callback selector and to be large enough (as part of loading immutability), but
	 it will be missing Alien until Alien is loaded.  So check if the specialObjectsArray is
	 of the expected size before recreating and slam Alien in if the specialObjectsArray
	 is already large enough."

	| index |
	self ~~ Alien ifTrue: [^self].

	index := 53.
	((self environment includesKey: #VMObjectIndices)
	 and: [((Smalltalk at: #VMObjectIndices) classPool at: #ClassAlien ifAbsent: []) ~= (index - 1)]) ifTrue:
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

{ #category : #libraries }
Alien class >> ensureLoaded: aLibraryName [ "<String> ^<Alien>"
	^LoadedLibraries
		at: aLibraryName
		ifAbsentPut:
			[| lib alien |
			 lib := self primLoadLibrary: aLibraryName.
			 alien := Alien new: WordSize.
			 WordSize = 4
				ifTrue: [alien unsignedLongAt: 1 put: lib]
				ifFalse: [alien unsignedLongLongAt: 1 put: lib].
			 alien]
]

{ #category : #'class initialization' }
Alien class >> ensureNotifiedAtStartup [
	"Alien needs to be notified at system startup to clear the LoadedLibraries table
	and otherwise sanitise Aliens with dangling pointers."
	self ~~ Alien ifTrue: [^self].
	(Smalltalk classNamed: #SessionManager)
		ifNotNil: [:sessionManagerClass|
			sessionManagerClass default
				registerSystemClassNamed: self name
				atPriority: 60]
		ifNil: [Smalltalk addToStartUpList: self after: Delay]
]

{ #category : #examples }
Alien class >> exampleCqsort [
	"Call the libc qsort function (which requires a callback)."
	"Alien exampleCqsort"
	"(Time millisecondsToRun: [100 timesRepeat: [Alien exampleCqsort]]) / 100.0"
	| cb rand nElements sizeofDouble values orig sort |
	rand := Random new.
	values := Alien newC: (nElements := 100) * (sizeofDouble := 8).
	1 to: values dataSize by: sizeofDouble do:
		[:i| values doubleAt: i put: rand next].
	orig := (1 to: values dataSize by: sizeofDouble) collect: [:i| values doubleAt: i].
	cb := Callback
			signature:  #(int (*)(const void *, const void *))
			block: [ :arg1 :arg2 | ((arg1 doubleAt: 1) - (arg2 doubleAt: 1)) sign].
	(Alien lookup: 'qsort' inLibrary: Alien libcName)
		primFFICallResult: nil
		with: values pointer
		with: nElements
		with: sizeofDouble
		with: cb thunk.
	sort := (1 to: values dataSize by: sizeofDouble) collect: [:i| values doubleAt: i].
	values free.
	^orig -> sort
]

{ #category : #examples }
Alien class >> examplePrintf [
	"Call the libc printf function."
	"Alien examplePrintf"
	| r s |
	(self lookup: 'printf' inLibrary: self libcName)
		primFFICallResult: (r := Alien forWord)
		withArguments: {(s := ('Hello World %d %x !!', (String with: Character lf)) asAlien) pointer.
						 123.
						 48879}.
	s free.
	^r signedWordAtWordIndex: 1
]

{ #category : #finalization }
Alien class >> finalize: mallocTag [
	self primFree: mallocTag
]

{ #category : #'instance creation' }
Alien class >> forPointer: address [ "<Integer>" "^<Alien>"
	"N.B.  This method assumes one is creating a pointer to already initialized data, hence initiaize is not sent.
	(Perhaps some alternative, eg. initializePointer, should be sent?"
	^(self basicNew: WordSize * 2)
		sizeFieldPut: 0;
		addressFieldPut: address;
		yourself
]

{ #category : #'instance creation' }
Alien class >> forWord [ "<Integer>" "^<Alien>"
	^(self basicNew: WordSize * 2)
		sizeFieldPut: WordSize;
		yourself
]

{ #category : #'class initialization' }
Alien class >> initialize [
	"Alien initialize"
	LoadedLibraries ifNil:
		[LoadedLibraries := Dictionary new].
	GCMallocedAliens ifNil:
		[GCMallocedAliens := AlienWeakTable newForOwner: self].
	WordSize := Smalltalk wordSize.
	lifoCallbackSemaphore := Semaphore new.
	self ensureInSpecialObjectsArray.
	self ensureNotifiedAtStartup
]

{ #category : #callbacks }
Alien class >> invokeCallback: thunkPtr "<Integer>" stack: stackPtr "<Integer>" registers: regsPtr "<Integer>" jmpbuf: jmpBufPtr [ "<Integer>" "^<FFICallbackReturnValue>"
	"The low-level entry-point for callbacks sent from the VM/IA32ABI plugin.
	 Return via primReturnFromContext:through:.  thisContext's sender is the
	 call-out context."
	| resultProxy |
	[resultProxy := Callback evaluateCallbackForThunk: thunkPtr stack: stackPtr]
		ifCurtailed: [self error: 'attempt to non-local return across a callback'].
	resultProxy ifNil:
		[(resultProxy := FFICallbackReturnValue new) returnInteger: 0].
	resultProxy primReturnFromContext: thisContext through: jmpBufPtr
]

{ #category : #callbacks }
Alien class >> invokeCallbackContext: vmCallbackContextAddress [ "<Integer>" "^<FFICallbackReturnValue>"
	"The low-level entry-point for callbacks sent from the VM/IA32ABI plugin.
	 Evaluate the callback corresponding to the thunk referenced by vmCallbackContextAddress,
	 a pointer to a VMCallbackContext32 or VMCallbackContext64, set up by the VM's thunkEntry
	 routine.  Return to C via primSignal:andReturnAs:fromContext:.  thisContext's sender is the
	 call-out context."
	| callbackAlien type |
	callbackAlien := (Smalltalk wordSize = 4
						ifTrue: [VMCallbackContext32]
						ifFalse: [VMCallbackContext64])
							atAddress: vmCallbackContextAddress.
	[type := Callback evaluateCallbackForContext: callbackAlien]
		ifCurtailed: [self error: 'attempt to non-local return across a callback'].
	type ifNil:
		[type := 1. callbackAlien wordResult: -1].
	"Now attempt to return to C.  The primitive will answer false if this is not the most recent Callback,
	 in which case we should wait on the lifoCallbackSemaphore which will be signalled when the most
	 recent callback successfully returns.  If this is the most recent callback the primitive will signal all
	 the processes waiting on the lifoCallbackSemaphore, one of which will be the next most recent.
	 Hence all nested callbacks will end up returning in the right order."
	[callbackAlien primSignal: lifoCallbackSemaphore andReturnAs: type fromContext: thisContext] whileFalse:
		[lifoCallbackSemaphore wait]
]

{ #category : #examples }
Alien class >> libcName [
	| platform |
	platform := SmalltalkImage current platformName.
	platform = 'Mac OS' ifTrue:
		[^SmalltalkImage current osVersion asInteger >= 1070
			ifTrue: ['libobjc.dylib']
			ifFalse:
				[SmalltalkImage current osVersion asInteger >= 1050
					ifTrue: ['libgcc_s.1.dylib']
					ifFalse: ['libc.dylib']]].
	platform = 'unix' ifTrue:
		[SmalltalkImage current osVersion = 'linux-gnu' ifTrue:
			[^'libc.so.6'].
		 ^'libc.so'].
	platform = 'Win32' ifTrue:
		[^'msvcrt.dll'].
	^nil
]

{ #category : #libraries }
Alien class >> lookup: symbol "<String>" inLibrary: libraryNameOrAlien [ "<String|Alien> ^<Alien>"
	^self forPointer: ((libraryNameOrAlien isString
						ifTrue: [self ensureLoaded: libraryNameOrAlien]
						ifFalse: [libraryNameOrAlien])
							primFindSymbol: symbol)
]

{ #category : #libraries }
Alien class >> lookupOrNil: symbol  "<String>" inLibrary: libraryName [ "<String> ^<Alien>"
	"Answer the address of symbol in libraryName, or nil if it is not in the library.
	Exceptions will be raised for invalid libraries, symbol names, etc."
	^[self lookup: symbol inLibrary: libraryName ]
		on: LibrarySymbolNotFoundError
		do: [:ex| nil]
]

{ #category : #'instance creation' }
Alien class >> new [
	"Answer a new instance of the default size whose data resides on the Smalltalk heap directly in the instance."
	^self new: self dataSize
]

{ #category : #'instance creation' }
Alien class >> new: byteSize [
	"Answer a new instance of the given byte size whose data resides on the Smalltalk heap directly in the instance."
	^(self basicNew: byteSize + WordSize)
		sizeFieldPut: byteSize;
		initialize
]

{ #category : #'instance creation' }
Alien class >> newC [
	"Answer a new instance of the default size whose data resides on the external C heap."
	^self newC: self dataSize
]

{ #category : #'instance creation' }
Alien class >> newC: byteSize [
	"Answer a new instance of the given byte size whose data resides on the external C heap.
	 As with instances residing on the Smalltalk heap the data is zero-filled."
	^(self basicNew: WordSize * 2)
		sizeFieldPut: byteSize negated;
		addressFieldPut: (self Ccalloc: byteSize);
		initialize
]

{ #category : #'instance creation' }
Alien class >> newCString: aString [ "<String> ^<Alien>"
	"Answer an Alien pointing to a null-terminated copy of aString on the external C heap."

	| size |
	size := aString size.
	^(Alien rawNewC: size + 1)
		replaceFrom: 1 to: size with: aString startingAt: 1;
		unsignedByteAt: size + 1 put: 0;
		yourself
]

{ #category : #'instance creation' }
Alien class >> newGC [
	"Answer a new instance of the default size whose data resides on the external C heap,
	 and whose data will be reclaimed automatically when the instance is garbage collected."
	^self newGC: self dataSize
]

{ #category : #'instance creation' }
Alien class >> newGC: byteSize [
	"Answer a new instance of the given byte size whose data resides on the external C heap,
	 and whose data will be reclaimed automatically when the instance is garbage collected.
	 As with instances residing on the Smalltalk heap the data is zero-filled."
	| alien address |
	(alien := self basicNew: WordSize * 2)
		sizeFieldPut: byteSize negated.
	address := self Ccalloc: byteSize.
	GCMallocedAliens add: alien finalizing: address.
	^alien
		addressFieldPut: address;
		initialize
]

{ #category : #'primitives-memory management' }
Alien class >> primFree: address [ "<Integer>"
	<primitive: 'primFree' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-library loading' }
Alien class >> primInLibrary: libraryHandle "<Alien>" findSymbol: symbolName [ "<String> ^<Integer>"
	<primitive: 'primInLibraryFindSymbol' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-library loading' }
Alien class >> primLoadLibrary: libraryName [ "<String> ^<Integer>"
	"Attempt to load the shared library/DLL named libraryName and
	 answer a handle which can be used in primFindSymbol:inLibrary:,
	 or fail if libraryName is invalid or libraryName cannot be found."
	<primitive: 'primLoadLibrary' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'instance creation' }
Alien class >> rawNewC: byteSize [
	"Answer a new instance of the given byte size whose data resides on the external C heap.
	 Unlike newC: the data is *not* zero-filled."
	^(self basicNew: WordSize * 2)
		sizeFieldPut: byteSize negated;
		addressFieldPut: (self Cmalloc: byteSize);
		initialize
]

{ #category : #'system startup' }
Alien class >> startUp: resuming [
	resuming ifTrue:
		[LoadedLibraries := Dictionary new: LoadedLibraries size.
		"Any addresses in the finalization registry at the time the image
		 comes up in a new session MUST NOT be finalized and should
		 immediately be discarded. These addresses are no longer valid."
		 GCMallocedAliens ifNotNil:
			[:gcMallocedAliens| gcMallocedAliens removeAll].
		"The io lock is off in the new session, so we shut down the process if it's running."
		ProcessIoEvents := false.
		WordSize := Smalltalk wordSize]
]

{ #category : #accessing }
Alien class >> wordSize [
	^WordSize
]

{ #category : #accessing }
Alien >> address [
	^self sizeField <= 0
		ifTrue: [self addressField]
		ifFalse: [self error: 'One cannot take the address of direct Alien instances; they may move']
]

{ #category : #'primitives-accessing' }
Alien >> addressField [ "^<Integer>"
	<primitive: 'primAddressField' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-accessing' }
Alien >> addressFieldPut: value [ "<Integer> ^<Integer>"
	<primitive: 'primAddressFieldPut' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #coercing }
Alien >> as: anAlienClass [
	"Attempt to coerce the receiver into a different class of Alien.  Performs minimal checks!! Use with caution!!"
	| alienClassDataSize |
	"Always attempt to access anAlienClass dataSize as a run-time check for anAlienClass actually being an Alien class."
	alienClassDataSize := anAlienClass dataSize.
	self isPointer ifFalse:
		[self dataSize >= alienClassDataSize ifFalse:
			[self error: 'receiver too small to coerce']].
	anAlienClass adoptInstance: self
]

{ #category : #coercing }
Alien >> asAlien [
	^self
]

{ #category : #converting }
Alien >> asSignedByte [ "^<Integer>"
	"Answer an Integer created from the first byte of the receiver interpreted as a signed value."
	
	^self signedByteAt: 1
]

{ #category : #converting }
Alien >> asSignedLong [ "^<Integer>"
	"Answer an Integer created from the first 4 bytes of the receiver interpreted as a signed value."
	
	^self signedLongAt: 1
]

{ #category : #converting }
Alien >> asSignedShort [ "^<Integer>"
	"Answer an Integer created from the first 2 bytes of the receiver interpreted as a signed value."
	
	^self signedShortAt: 1
]

{ #category : #converting }
Alien >> asUnsignedByte [ "^<Integer>"
	"Answer an Integer created from the first byte of the receiver interpreted as an unsigned value."
	
	^self unsignedByteAt: 1
]

{ #category : #converting }
Alien >> asUnsignedLong [ "^<Integer>"
	"Answer an Integer created from the first 4 bytes of the receiver interpreted as an unsigned value."
	
	^self unsignedLongAt: 1
]

{ #category : #converting }
Alien >> asUnsignedShort [ "^<Integer>"
	"Answer an Integer created from the first 2 bytes of the receiver interpreted as an unsigned value."
	
	^self unsignedShortAt: 1
]

{ #category : #'primitives-accessing' }
Alien >> copyInto: aByteObject "<Alien | indexableByteSubclass>" from: start "<Integer>" to: stop "<Integer>" in: replacement "<Alien | indexableByteSubclass>" startingAt: repStart [ "<Integer>" "^<self>"
	<primitive: 'primAlienReplace' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #accessing }
Alien >> dataSize [
	^self sizeField abs
]

{ #category : #'primitives-accessing' }
Alien >> doubleAt: index [ "<Integer> ^<Float>"
	<primitive: 'primDoubleAt' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-accessing' }
Alien >> doubleAt: index "<Integer>" put: value [ "<Float | Integer> ^<Float | Integer>"
	<primitive: 'primDoubleAtPut' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-accessing' }
Alien >> floatAt: index [ "<Integer> ^<Float>"
	<primitive: 'primFloatAt' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-accessing' }
Alien >> floatAt: index "<Integer>" put: value [ "<Float | Integer> ^<Float | Integer>"
	<primitive: 'primFloatAtPut' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'memory management' }
Alien >> free [
	self primFree
]

{ #category : #accessing }
Alien >> freeAfter: aBlock [
	"The receiver must be a pointer to a previously allocated external C heap memory block. Invoke aBlock passing the receiver as the argument, then free the receiver. Allows automatic allocation and freeing of C artifacts using patters such as
	(Alien newCString: 'foo') freeAfter: [:cstring | ...use cstring here...]."
	
	^[aBlock value: self] ensure: [self free]
]

{ #category : #initialization }
Alien >> initialize [
	"Subclasses should redefine this method to perform initializations on instance creation"
	"Overrides ProtoObjects' method for explicitness."
]

{ #category : #testing }
Alien >> isPointer [
	^self sizeField = 0
]

{ #category : #accessing }
Alien >> pointer [
	"Answer a pointer to the receiver in the form of an Alien.
	 This will fail if the receiver is not indirect."
	self isPointer ifTrue:
		[self error: 'One cannot take the address of pointer Alien instances; they may move, and are already pointers'].
	^self class forPointer: self address
]

{ #category : #'primitives-callouts' }
Alien >> primFFICallResult: result [ "<Alien> ^<Alien>"
	<primitive: 'primCallOutIntegralReturn' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-callouts' }
Alien >> primFFICallResult: result "<Alien>" with: firstArg [ "<Alien | Integer> ^<Alien>"
	<primitive: 'primCallOutIntegralReturn' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-callouts' }
Alien >> primFFICallResult: result "<Alien>" with: firstArg "<Alien | Integer>" with: secondArg [ "<Alien | Integer> ^<Alien>"
	<primitive: 'primCallOutIntegralReturn' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-callouts' }
Alien >> primFFICallResult: result "<Alien>" with: firstArg "<Alien | Integer>" with: secondArg "<Alien | Integer>" with: thirdArg [ "<Alien | SmallInteger> ^<Alien>"
	<primitive: 'primCallOutIntegralReturn' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-callouts' }
Alien >> primFFICallResult: result "<Alien>" with: firstArg "<Alien | Integer>" with: secondArg "<Alien | Integer>" with: thirdArg "<Alien | Integer>" with: fourthArg [ "<Alien | Integer> ^<Alien>"
	<primitive: 'primCallOutIntegralReturn' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-callouts' }
Alien >> primFFICallResult: result "<Alien>" with: firstArg "<Alien | Integer>" with: secondArg "<Alien | Integer>" with: thirdArg "<Alien | Integer>" with: fourthArg "<Alien | Integer>" with: fifthArg [ "<Alien | Integer> ^<Alien>"
	<primitive: 'primCallOutIntegralReturn' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-callouts' }
Alien >> primFFICallResult: result "<Alien>" with: firstArg "<Alien | Integer>" with: secondArg "<Alien | Integer>" with: thirdArg "<Alien | Integer>" with: fourthArg "<Alien | Integer>" with: fifthArg "<Alien | Integer>" with: sixthArg [ "<Alien | Integer> ^<Alien>"
	<primitive: 'primCallOutIntegralReturn' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-callouts' }
Alien >> primFFICallResult: result "<Alien>" with: firstArg "<Alien | Integer>" with: secondArg "<Alien | Integer>" with: thirdArg "<Alien | Integer>" with: fourthArg "<Alien | Integer>" with: fifthArg "<Alien | Integer>" with: sixthArg "<Alien | Integer>" with: arg7 [ "<Alien | Integer>" "^<Alien>"
	<primitive: 'primCallOutIntegralReturn' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-callouts' }
Alien >> primFFICallResult: result "<Alien>" with: firstArg "<Alien | Integer>" with: secondArg "<Alien | Integer>" with: thirdArg "<Alien | Integer>" with: fourthArg "<Alien | Integer>" with: fifthArg "<Alien | Integer>" with: sixthArg "<Alien | Integer>" with: arg7 "<Alien | Integer>" with: arg8 [ "<Alien | Integer>" "^<Alien>"
	<primitive: 'primCallOutIntegralReturn' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-callouts' }
Alien >> primFFICallResult: result "<Alien>" with: firstArg "<Alien | Integer>" with: secondArg "<Alien | Integer>" with: thirdArg "<Alien | Integer>" with: fourthArg "<Alien | Integer>" with: fifthArg "<Alien | Integer>" with: sixthArg "<Alien | Integer>" with: arg7 "<Alien | Integer>" with: arg8 "<Alien | Integer>" with: arg9 [ "<Alien | Integer>" "^<Alien>"
	<primitive: 'primCallOutIntegralReturn' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-callouts' }
Alien >> primFFICallResult: result "<Alien>" with: firstArg "<Alien | Integer>" with: secondArg "<Alien | Integer>" with: thirdArg "<Alien | Integer>" with: fourthArg "<Alien | Integer>" with: fifthArg "<Alien | Integer>" with: sixthArg "<Alien | Integer>" with: arg7 "<Alien | Integer>" with: arg8 "<Alien | Integer>" with: arg9 "<Alien | Integer>" with: arg10 [ "<Alien | Integer>" "^<Alien>"
	<primitive: 'primCallOutIntegralReturn' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-callouts' }
Alien >> primFFICallResult: result "<Alien>" with: firstArg "<Alien | Integer>" with: secondArg "<Alien | Integer>" with: thirdArg "<Alien | Integer>" with: fourthArg "<Alien | Integer>" with: fifthArg "<Alien | Integer>" with: sixthArg "<Alien | Integer>" with: arg7 "<Alien | Integer>" with: arg8 "<Alien | Integer>" with: arg9 "<Alien | Integer>" with: arg10 "<Alien | Integer>" with: arg11 [ "<Alien | Integer>" "^<Alien>"
	<primitive: 'primCallOutIntegralReturn' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-callouts' }
Alien >> primFFICallResult: result "<Alien>" with: firstArg "<Alien | Integer>" with: secondArg "<Alien | Integer>" with: thirdArg "<Alien | Integer>" with: fourthArg "<Alien | Integer>" with: fifthArg "<Alien | Integer>" with: sixthArg "<Alien | Integer>" with: arg7 "<Alien | Integer>" with: arg8 "<Alien | Integer>" with: arg9 "<Alien | Integer>" with: arg10 "<Alien | Integer>" with: arg11 "<Alien | Integer>" with: arg12 [ "<Alien | Integer>" "^<Alien>"
	<primitive: 'primCallOutIntegralReturn' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-callouts' }
Alien >> primFFICallResult: result "<Alien>" withArguments: args [ "<Array of: Alien | Integer> ^<Alien>"
	<primitive: 'primVarArgsCallOutIntegralReturn' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-library loading' }
Alien >> primFindSymbol: symbolName [ "<String> ^<Integer>"
	"The receiver is assumed to be a proxy whose first field contans the handle of a library
	 answered by primLoadLibrary.  Attempt to find symbolName in the library and answer
	 its address, or fail if the receiver is invalid or the symbol cannot be found."
	<primitive: 'primInLibraryFindSymbol' module: 'IA32ABI' error: errorCode>
	^errorCode == #'not found'
		ifTrue: [LibrarySymbolNotFoundError signal]
		ifFalse: [self primitiveFailed]
]

{ #category : #'primitives-memory management' }
Alien >> primFree [
	"Free the memory referenced by the receiver, or fail if
	 the receiver is direct or the indirect pointer is invalid.
	 Note that if successful the address and size fields are
	 zero'ed, effectively preventing double-freeing hazards."
	<primitive: 'primBoxedFree' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-accessing' }
Alien >> replaceFrom: start "<Integer>" to: stop "<Integer>" with: replacement "<Alien | indexableByteSubclass>" startingAt: repStart [ "<Integer>" "^<self>"
	<primitive: 'primAlienReplace' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-accessing' }
Alien >> signedByteAt: index [ "<Integer> ^<Integer>"
	<primitive: 'primSignedByteAt' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-accessing' }
Alien >> signedByteAt: index "<Integer>" put: value [ "<Integer> ^<Integer>"
	<primitive: 'primSignedByteAtPut' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-accessing' }
Alien >> signedLongAt: index [ "<Integer> ^<Integer>"
	<primitive: 'primSignedLongAt' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-accessing' }
Alien >> signedLongAt: index "<Integer>" put: value [ "<Integer> ^<Integer>"
	<primitive: 'primSignedLongAtPut' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-accessing' }
Alien >> signedLongLongAt: index [ "<Integer> ^<Integer>"
	<primitive: 'primSignedLongLongAt' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-accessing' }
Alien >> signedLongLongAt: index "<Integer>" put: value [ "<Integer> ^<Integer>"
	<primitive: 'primSignedLongLongAtPut' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-accessing' }
Alien >> signedShortAt: index [ "<Integer> ^<Integer>"
	<primitive: 'primSignedShortAt' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-accessing' }
Alien >> signedShortAt: index "<Integer>" put: value [ "<Integer> ^<Integer>"
	<primitive: 'primSignedShortAtPut' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #accessing }
Alien >> signedWordAtWordIndex: wordIndex [
	^WordSize = 4
		ifTrue: [self signedLongAt: wordIndex - 1 * 4 + 1]
		ifFalse: [self signedLongLongAt: wordIndex - 1 * 8 + 1]
]

{ #category : #'primitives-accessing' }
Alien >> sizeField [ "^<Integer>"
	<primitive: 'primSizeField' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-accessing' }
Alien >> sizeFieldPut: size [ "<Integer> ^<Integer>"
	<primitive: 'primSizeFieldPut' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #printing }
Alien >> storeOn: aStream [
	| sizeField |
	(sizeField := self sizeField) ~= 0 ifTrue:
		[aStream nextPut: $(].
	aStream nextPut: $(; print: self class.
	sizeField = 0
		ifTrue:
			[aStream nextPutAll: ' forPointer: '; nextPutAll: self addressField storeStringHex]
		ifFalse:
			[aStream
				nextPutAll: (sizeField < 0 ifTrue: [' newC: '] ifFalse: [' new: ']);
				print: (sizeField := sizeField abs); nextPut: $).
			 1 to: sizeField by: 4 do:
				[:i| | v |
				(v := self unsignedLongAt: i) ~= 0 ifTrue:
					[aStream
						crtab;
						nextPutAll: 'unsignedLongAt: '; print: i;
						nextPutAll: ' put: '; print: v; nextPut: $;]].
			 aStream crtab; nextPutAll: #yourself].
	aStream nextPut: $)

	"{	(Alien forPointer: 0) storeString.
		(Alien new: 8) storeString.
		(Alien newC: 8) storeString }"
]

{ #category : #accessing }
Alien >> strcpy [ "^<ByteString>"
	^self strcpyFrom: 1
]

{ #category : #accessing }
Alien >> strcpyFrom: startIndex [ "<Integer> ^<ByteString>"
	| length string |
	length := self strlenStartingAt: startIndex.
	self copyInto: (string := ByteString new: length)
		from: 1 to: length
		in: self startingAt: startIndex.
	^string
]

{ #category : #accessing }
Alien >> strcpyUTF8 [ "^<String>"
	^self strcpyUTF8From: 1
]

{ #category : #accessing }
Alien >> strcpyUTF8From: startIndex [ "<Integer> ^<String>"
	| length bytes |
	length := self strlenStartingAt: startIndex.
	self copyInto: (bytes := ByteArray new: length)
		from: 1 to: length
		in: self startingAt: startIndex.
	^bytes utf8ToIso
]

{ #category : #accessing }
Alien >> strlen [
	^self strlenStartingAt: 1
]

{ #category : #'primitives-accessing' }
Alien >> strlenStartingAt: index [ "<Integer> ^<Integer>"
	<primitive: 'primStrlenFromStartIndex' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-accessing' }
Alien >> strlenThroughPointerAt: index [ "<Integer> ^<Integer>"
	<primitive: 'primStrlenThroughPointerAtIndex' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-accessing' }
Alien >> unsignedByteAt: index [ "<Integer> ^<Integer>"
	<primitive: 'primUnsignedByteAt' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-accessing' }
Alien >> unsignedByteAt: index "<Integer>" put: value [ "<Integer> ^<Integer>"
	<primitive: 'primUnsignedByteAtPut' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-accessing' }
Alien >> unsignedLongAt: index [ "<Integer> ^<Integer>"
	<primitive: 'primUnsignedLongAt' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-accessing' }
Alien >> unsignedLongAt: index "<Integer>" put: value [ "<Integer> ^<Integer>"
	<primitive: 'primUnsignedLongAtPut' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-accessing' }
Alien >> unsignedLongLongAt: index [ "<Integer> ^<Integer>"
	<primitive: 'primUnsignedLongLongAt' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-accessing' }
Alien >> unsignedLongLongAt: index "<Integer>" put: value [ "<Integer> ^<Integer>"
	<primitive: 'primUnsignedLongLongAtPut' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-accessing' }
Alien >> unsignedShortAt: index [ "<Integer> ^<Integer>"
	<primitive: 'primUnsignedShortAt' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'primitives-accessing' }
Alien >> unsignedShortAt: index "<Integer>" put: value [ "<Integer> ^<Integer>"
	<primitive: 'primUnsignedShortAtPut' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]
