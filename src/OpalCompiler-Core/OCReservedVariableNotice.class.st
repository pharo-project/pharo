"
I am a notice raised when a reserved word (e.g. self, super, thisContext) is used as a parameter or temporary variable name.

I am an error by default, and I become a warning when the compiler permits shadowing or faults.
"
Class {
	#name : 'OCReservedVariableNotice',
	#superclass : 'OCErrorNotice',
	#category : 'OpalCompiler-Core-FrontEnd',
	#package : 'OpalCompiler-Core',
	#tag : 'FrontEnd'
}

{ #category : 'testing' }
OCReservedVariableNotice >> isErrorOn: anOpalCompiler [

	^ anOpalCompiler isStrict
]
