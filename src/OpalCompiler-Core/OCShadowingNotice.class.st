"
I am a notice raised when a variable (argument or temporary) shadows another variable that was already visible (e.g. a global, an instance variable or an outer temporary).

I am a warning by default, and I become an error when the compiler explicitly forbids shadowing (`permitShadowing: false`).
"
Class {
	#name : 'OCShadowingNotice',
	#superclass : 'OCWarningNotice',
	#category : 'OpalCompiler-Core-FrontEnd',
	#package : 'OpalCompiler-Core',
	#tag : 'FrontEnd'
}

{ #category : 'testing' }
OCShadowingNotice >> isErrorOn: anOpalCompiler [

	^ anOpalCompiler isStrict
]
