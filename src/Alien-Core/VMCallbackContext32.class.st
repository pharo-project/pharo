"
A VMCallbackContext32 is an Alien for the 32-bit version of VMCallbackContext (defined in the VMMaker package), a struct that maintains all the necessary context for a callback.

typedef struct {
    void *thunkp;
    char *stackptr;
    long *intRegArgs;
    double *floatRegArgs;
    void *savedCStackPointer;
    void *savedCFramePointer;
    union {
                            long vallong;
                            struct { int low, high; } valleint64;
                            struct { int high, low; } valbeint64;
                            double valflt64;
                            struct { void *addr; long size; } valstruct;
                        }   rvs;
    jmp_buf trampoline;
 } VMCallbackContext;

Instance Variables

"
Class {
	#name : #VMCallbackContext32,
	#superclass : #Alien,
	#type : #bytes,
	#category : #'Alien-Core'
}

{ #category : #'instance creation' }
VMCallbackContext32 class >> dataSize [
	"We only access the fields before trampoline, so this is fine."
	^32
]

{ #category : #accessing }
VMCallbackContext32 >> floatResult: aFloat [
	self doubleAt: 25 put: aFloat.
	^3
]

{ #category : #accessing }
VMCallbackContext32 >> floatregargsp [
	^Alien forPointer: (self unsignedLongAt: 13)
]

{ #category : #accessing }
VMCallbackContext32 >> intregargsp [
	^Alien forPointer: (self unsignedLongAt: 9)
]

{ #category : #primitives }
VMCallbackContext32 >> primReturnAs: typeCode "<SmallInteger>" fromContext: context [ "<MethodContext>"
	<primitive: 'primReturnAsFromContextThrough' module: 'IA32ABI' error: ec>
	^self primitiveFailed
]

{ #category : #primitives }
VMCallbackContext32 >> primSignal: aSemaphore "<Semaphore>" andReturnAs: typeCode "<SmallInteger>" fromContext: context [ "<MethodContext>"
	<primitive: 'primReturnAsFromContextThrough' module: 'IA32ABI' error: ec>
	^ self primitiveFailed
]

{ #category : #'accessing-legacy' }
VMCallbackContext32 >> rawStackp [
	^self unsignedLongAt: 5
]

{ #category : #'accessing-legacy' }
VMCallbackContext32 >> returnDouble: f [
	^self floatResult: f
]

{ #category : #'accessing-legacy' }
VMCallbackContext32 >> returnInteger: n [
	^self wordResult: n
]

{ #category : #accessing }
VMCallbackContext32 >> singleFloatResult: aFloat [
	self floatAt: 25 put: aFloat.
	^3
]

{ #category : #accessing }
VMCallbackContext32 >> stackp [
	^Alien forPointer: (self unsignedLongAt: 5)
]

{ #category : #accessing }
VMCallbackContext32 >> thunkp [
	^self unsignedLongAt: 1
]

{ #category : #accessing }
VMCallbackContext32 >> wordResult: aValue [
	"Accept any value in the -2^31 to 2^32-1 range or booleans."
	| anInteger |
	anInteger := aValue isInteger
						ifTrue: [aValue]
						ifFalse:  "Will provoke a mustBeBoolean error if not a boolean."
							[aValue ifTrue: [1] ifFalse: [0]].
	anInteger >= 0
		ifTrue: [self unsignedLongAt: 25 put: anInteger]
		ifFalse: [self signedLongAt: 25 put: anInteger].
	^1
]

{ #category : #accessing }
VMCallbackContext32 >> wordSize [
	^4
]
