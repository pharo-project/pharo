"
I am FFIVariableNameNotFound, an exception to indicate that a variable is not correctly used inside an ffi function
"
Class {
	#name : #FFIVariableNameNotFound,
	#superclass : #NotFound,
	#category : #'UnifiedFFI-Exceptions'
}

{ #category : #private }
FFIVariableNameNotFound >> standardMessageText [
	"Generate a standard textual description"

	^ String
		streamContents: [ :stream | 
			stream << 'Could not find accessor for variable named "'.
			stream print: self object.
			stream << '"' ]
]
