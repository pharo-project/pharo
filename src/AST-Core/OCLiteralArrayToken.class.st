"
I am the start of a literal array, normal literal arrays `#()` or byte arrays `#[]`.

"
Class {
	#name : 'OCLiteralArrayToken',
	#superclass : 'OCValueToken',
	#category : 'AST-Core-Tokens',
	#package : 'AST-Core',
	#tag : 'Tokens'
}

{ #category : 'testing' }
OCLiteralArrayToken >> isForByteArray [
	^value last = $[
]

{ #category : 'testing' }
OCLiteralArrayToken >> isLiteralArrayToken [
	^true
]
