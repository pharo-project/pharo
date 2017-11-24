"
I am PrimitiveFailed, an exception signaled when a primitive fails.
"
Class {
	#name : #PrimitiveFailed,
	#superclass : #SelectorException,
	#category : #'Kernel-Exceptions'
}

{ #category : #printing }
PrimitiveFailed >> standardMessageText [
	"Generate a standard textual description"
	
	^ String streamContents: [ :stream |
		stream << 'primitive '.
		stream print: self selector.
		stream << ' in '.
		stream print: self signaler class.
		stream << ' failed' ]
]
