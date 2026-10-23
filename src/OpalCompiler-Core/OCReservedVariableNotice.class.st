"
I am a notice raised when a reserved word (e.g. self, super, thisContext) is used as a parameter or temporary variable name.

I am an a warning by default. I become an error when the compiler is in strict mode.
"
Class {
	#name : 'OCReservedVariableNotice',
	#superclass : 'OCWarningNotice',
	#category : 'OpalCompiler-Core-FrontEnd',
	#package : 'OpalCompiler-Core',
	#tag : 'FrontEnd'
}

{ #category : 'testing' }
OCReservedVariableNotice >> isErrorOn: anOpalCompiler [

	^ anOpalCompiler isStrict
]
