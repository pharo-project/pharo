"
Obsolete support for old-style callbacks.  An instance of FFICallbackReturnValue specifies a return value to be passed to a callback callee.  It is intended to have overlaid the following struct:
/*
 * Returning values from callbacks is done through a CallBackReturnSpec
 * which contains a type tag and values.  It is designed to be overlaid upon
 * an FFICallbackReturnProxy created at the Smalltalk level to return values.
 */
typedef struct {
    long type;
# define retint32  0 
# define retint64  1
# define retdouble 2
# define retstruct 3
    long _pad; /* so no doubt that valflt64 & valint32 et al are at byte 8 */
    union {
        long valint32;
        struct { long low, high; } valint64;
        double valflt64;
        struct { void *addr; long size; } valstruct;
    } rvs;
} CallBackReturnSpec;

"
Class {
	#name : #FFICallbackReturnValue,
	#superclass : #Alien,
	#type : #bytes,
	#category : #'Alien-Core'
}

{ #category : #'instance creation' }
FFICallbackReturnValue class >> dataSize [
	"See class comment"
	^16
]

{ #category : #'primitives-callbacks' }
FFICallbackReturnValue >> primReturnFromContext: callbackContext "<ContextPart>" through: jmpbuf [ "<Integer>" "^not reached"
	"Return a value, as described by the receiver, to some C callee that has made a
	callback.   Reestablish the interpreter's state prior to the callback so that either
	the callout can return or subsequent callbacks can occur.  callbackContext is an
	activation of invokeCallback:stack:registers:jmpbuf:.  Its sender is the interpreter's
	state prior to the callback"
	<primitive: 'primReturnFromContextThrough' module: 'IA32ABI' error: errorCode>
	self primitiveFailed
]

{ #category : #'instance initialization' }
FFICallbackReturnValue >> returnDouble: f [
	self signedLongAt: self typeOffset put: 2.
	self doubleAt: self valueOffset put: f asFloat
]

{ #category : #'instance initialization' }
FFICallbackReturnValue >> returnInteger: n [
	self signedLongAt: self typeOffset put: 0.
	self signedLongAt: self valueOffset put: n asInteger
]

{ #category : #'field offsets' }
FFICallbackReturnValue >> typeOffset [
	^1 "0"
]

{ #category : #'field offsets' }
FFICallbackReturnValue >> valueOffset [
	^9 "8"
]
