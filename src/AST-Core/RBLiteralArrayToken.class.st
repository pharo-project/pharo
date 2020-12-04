"
I am the start of a literal array, normal literal arrays `#()` or byte arrays `#[]`.

"
Class {
	#name : #RBLiteralArrayToken,
	#superclass : #RBValueToken,
	#category : #'AST-Core-Tokens'
}

{ #category : #testing }
RBLiteralArrayToken >> isForByteArray [
	^value last = $[
]

{ #category : #testing }
RBLiteralArrayToken >> isLiteralArrayToken [
	^true
]
