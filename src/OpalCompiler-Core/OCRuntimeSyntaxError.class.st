"
When compiling syntactically incorrect code, we compile raising this exception.

This way the debugger opens and the programmer can easily fix the faulty method
"
Class {
	#name : 'OCRuntimeSyntaxError',
	#superclass : 'Error',
	#category : 'OpalCompiler-Core-Exceptions',
	#package : 'OpalCompiler-Core',
	#tag : 'Exceptions'
}

{ #category : 'signalling' }
OCRuntimeSyntaxError class >> signalSyntaxError: aString [
	"we use signalSyntaxError: instead of signal: so we can quickly check
	compiledMethods for syntax errors by checking the literals"
	^ self signal: aString
]
