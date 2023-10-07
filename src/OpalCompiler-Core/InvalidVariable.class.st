"
I'm a variable (or maybe not?) declared in a AST node syntactic errors.

I exists only to track information on bad variables in a degraded mode, and avoid noisy and useless errors and warnings.
Once the AST is fixed I will be no more be present, so ignore me.
"
Class {
	#name : 'InvalidVariable',
	#superclass : 'Variable',
	#category : 'OpalCompiler-Core-Semantics',
	#package : 'OpalCompiler-Core',
	#tag : 'Semantics'
}

{ #category : 'testing' }
InvalidVariable >> allowsShadowing [
	^true
]

{ #category : 'testing' }
InvalidVariable >> isInvalidVariable [
	^true
]

{ #category : 'testing' }
InvalidVariable >> isUsed [
	"Not really used, but need to silence warnings"

	^ true
]
