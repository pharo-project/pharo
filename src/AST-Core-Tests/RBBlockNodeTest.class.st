Class {
	#name : #RBBlockNodeTest,
	#superclass : #RBParseTreeTest,
	#category : #'AST-Core-Tests-Nodes'
}

{ #category : #tests }
RBBlockNodeTest >> testConstantValue [

	self assert: [  ] sourceNode constantValue equals: nil.
	self assert: [ 1 ] sourceNode constantValue equals: 1.
	self should: [[ ^1 ] sourceNode constantValue] raise: TestResult error
]

{ #category : #tests }
RBBlockNodeTest >> testHasNonLocalReturn [

	self deny: [  ] sourceNode hasNonLocalReturn.
	self deny: [ [  ] ] sourceNode hasNonLocalReturn.
	self assert: [ ^ 1 ] sourceNode hasNonLocalReturn.
	self assert: [ [ ^ 1 ] ] sourceNode hasNonLocalReturn.
	self assert: [ { 1 . ^2 }  ] sourceNode hasNonLocalReturn.
	self assert: (self class>>#testHasNonLocalReturn) ast hasNonLocalReturn.
	"return on the level of the method is a local return"
	self deny: (Object>>#yourself) ast hasNonLocalReturn.
	self deny: (Object>>#halt) ast hasNonLocalReturn
]

{ #category : #tests }
RBBlockNodeTest >> testIsClean [
	| escpWrite escpRead |
	escpRead := escpWrite := 1.
	self deny: [ self yourself ] sourceNode isClean.
	self deny: [ ^ 1 ] sourceNode isClean.
	self deny: [ { 1 . ^2 }  ] sourceNode isClean.
	self deny: [ testSelector foo ] sourceNode isClean.
	self deny: [ escpRead foo ] sourceNode isClean.
	self deny: [ escpWrite := 2 ] sourceNode isClean.

	self deny: [[ self yourself ]] sourceNode isClean.
	self deny: [[ ^ 1 ]] sourceNode isClean.
	self deny: [[ testSelector foo ]] sourceNode isClean.
	self deny: [[ escpRead foo ]] sourceNode isClean.
	self deny: [[ escpWrite := 2 ]] sourceNode isClean.
	self assert: [  ] sourceNode isClean.
	self assert: [ thisContext ] sourceNode isClean.
	self assert: [ 1 + 2 ] sourceNode isClean.
	self assert: [ :a | a + 2 ] sourceNode isClean.
	self assert: [ :a :b | a + b + 3 ] sourceNode isClean.
	self assert: [ | a | a := 1. a + 3 ] sourceNode isClean.

	self assert: [ true ifTrue: [  ] ]  sourceNode isClean.
	"optimized blocks"
	self assert: [
		| local2 | true ifTrue: [local2 := 1 ] ] isClean. "optimized blocks are clean"
	self deny: [ true ifTrue: [escpWrite := 1 ] ] isClean. "But not if we have a real escape"
]

{ #category : #tests }
RBBlockNodeTest >> testIsConstant [

	self assert: [  ] sourceNode isConstant.
	self assert: [ 1 ] sourceNode isConstant.
	self deny: [ ^1 ] sourceNode isConstant.
	self deny: [ 1 sin. 1 ] sourceNode isConstant
]

{ #category : #tests }
RBBlockNodeTest >> testNumArgs [
	self assert: [  ] sourceNode numArgs equals: 0.
	self assert: [:a  |  ] sourceNode numArgs equals: 1.
	self assert: [:a :b  |  ] sourceNode numArgs equals:  [:a :b  |  ] numArgs
]
