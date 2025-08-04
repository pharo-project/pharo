"
I am a ridiculous alternate parser, that replaces any literal symbol #dontReturnThis with the symbol #expectedReturn.

It's the simplest change to the compiler that I could imagine that doesn't interfere with the fact that the class-side `compilerClass` method can't be interfered with.

Note that  I only override a single private method, so changes in the superclass must be reflected here.
"
Class {
	#name : 'OCTargetCompilerParser',
	#superclass : 'OCParser',
	#category : 'OpalCompiler-Tests-Semantic',
	#package : 'OpalCompiler-Tests',
	#tag : 'Semantic'
}

{ #category : 'private - parsing' }
OCTargetCompilerParser >> parsePrimitiveLiteral [
	"Creates a literal value node from a literal token."
	| token value |
	token := currentToken.
	self step.
	value := token value.
	value == #dontReturnThis ifTrue: [ value := #expectedReturn ].

	^self literalValueNodeClass
		value: value
		start: token start
		stop: token stop
		source: token source
]
