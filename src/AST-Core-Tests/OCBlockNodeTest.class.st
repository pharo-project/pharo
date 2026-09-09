Class {
	#name : 'OCBlockNodeTest',
	#superclass : 'OCParseTreeTest',
	#category : 'AST-Core-Tests-Nodes',
	#package : 'AST-Core-Tests',
	#tag : 'Nodes'
}

{ #category : 'tests' }
OCBlockNodeTest >> testConstantValue [

	self assert: [  ] sourceNode constantValue equals: nil.
	self assert: [ 1 ] sourceNode constantValue equals: 1.
	self should: [[ ^1 ] sourceNode constantValue] raise: TestResult error
]

{ #category : 'tests' }
OCBlockNodeTest >> testHasNonLocalReturn [

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

{ #category : 'tests' }
OCBlockNodeTest >> testIsClean [
	| escpWrite escpRead |
	escpRead := escpWrite := 1.
	self deny: [ self yourself ] sourceNode scope isClean.
	self deny: [ ^ 1 ] sourceNode scope isClean.
	self deny: [ { 1 . ^2 }  ] sourceNode scope isClean.
	self deny: [ testSelector foo ] sourceNode scope isClean.
	self deny: [ escpRead foo ] sourceNode scope isClean.
	self deny: [ escpWrite := 2 ] sourceNode scope isClean.

	self deny: [[ self yourself ]] sourceNode scope isClean.
	self deny: [[ ^ 1 ]] sourceNode scope isClean.
	self deny: [[ testSelector foo ]] sourceNode scope isClean.
	self deny: [[ escpRead foo ]] sourceNode scope isClean.
	self deny: [[ escpWrite := 2 ]] sourceNode scope isClean.
	self assert: [  ] sourceNode scope isClean.
	self assert: [ thisContext ] sourceNode scope isClean.
	self assert: [ 1 + 2 ] sourceNode scope isClean.
	self assert: [ :a | a + 2 ] sourceNode scope isClean.
	self assert: [ :a :b | a + b + 3 ] sourceNode scope isClean.
	self assert: [ | a | a := 1. a + 3 ] sourceNode scope isClean.

	self assert: [ true ifTrue: [  ] ]  sourceNode scope isClean.
	"optimized blocks"
	self assert: [
		| local2 | true ifTrue: [local2 := 1 ] ] isClean. "optimized blocks are clean"
	self deny: [ true ifTrue: [escpWrite := 1 ] ] isClean. "But not if we have a real escape"
]

{ #category : 'tests' }
OCBlockNodeTest >> testIsConstant [

	self assert: [  ] sourceNode isConstant.
	self assert: [ 1 ] sourceNode isConstant.
	self deny: [ ^1 ] sourceNode isConstant.
	"only one statement is allowed, even if the first is constant"
	self deny: [ 1 sin. 1 ] sourceNode isConstant.
	self deny: [ 1 . 1 ] sourceNode isConstant
]

{ #category : 'tests' }
OCBlockNodeTest >> testNumArgs [
	self assert: [  ] sourceNode numArgs equals: 0.
	self assert: [:a  |  ] sourceNode numArgs equals: 1.
	self assert: [:a :b  |  ] sourceNode numArgs equals:  [:a :b  |  ] numArgs
]
