Class {
	#name : #OCASTBlockTranslatorTest,
	#superclass : #OCASTTranslatorTest,
	#category : #'OpalCompiler-Tests-AST'
}

{ #category : #'tests-readvariables' }
OCASTBlockTranslatorTest >> testBlockCapturesVariableNotValue [
	
	| result |
	"Test that the block captures the variable and not the value at the moment of the closure"
	result := self testSource: 'example
	| t block |
	t := 1.
	block := [ t ].
	t := 2.
	^ block value'.
	
	self assert: result equals: 2
]

{ #category : #'tests-writevariables' }
OCASTBlockTranslatorTest >> testBlockExplicitReturn [
	
	| result |
	result := self testSource: 'example
	[ ^ 5 ] value.
	^ 2'.
	
	self assert: result equals: 5
]

{ #category : #'tests-arguments' }
OCASTBlockTranslatorTest >> testBlockWithArgumentReturnsLastExpression [
	
	| result |
	result := self testSource: 'example
	^ [ :arg | arg ] value: 7'.
	
	self assert: result equals: 7
]

{ #category : #'tests-readvariables' }
OCASTBlockTranslatorTest >> testBlockWithExternalTempReturnsLastExpression [
	
	| result |
	result := self testSource: 'example
	| t |
	t := 1.
	^ [ t ] value'.
	
	self assert: result equals: 1
]

{ #category : #'tests-arguments' }
OCASTBlockTranslatorTest >> testBlockWithFiveArgumentReturnsLastExpression [
	
	| result |
	result := self testSource: 'example
	^ [ :arg1 :arg2 :arg3 :arg4 :arg5 | arg1 + arg2 + arg3 + arg4 + arg5 ] 
		valueWithArguments: #( 2 3 4 5 6 )'.
	
	self assert: result equals: 20
]

{ #category : #'tests-arguments' }
OCASTBlockTranslatorTest >> testBlockWithFourArgumentReturnsLastExpression [
	
	| result |
	result := self testSource: 'example
	^ [ :arg1 :arg2 :arg3 :arg4 | arg1 + arg2 + arg3 + arg4 ] value: 2 value: 3 value: 4 value: 5'.
	
	self assert: result equals: 14
]

{ #category : #'tests-readvariables' }
OCASTBlockTranslatorTest >> testBlockWithInstanceVariableReturnsLastExpression [
	
	| result value |
	value := Object new.
	instance iVar: value.
	
	result := self testSource: 'example
	^ [ iVar ] value'.
	
	self assert: result equals: value
]

{ #category : #'tests-literals' }
OCASTBlockTranslatorTest >> testBlockWithLiteralReturnsLastExpression [
	
	| result |
	result := self testSource: 'example
	^ [ 1 ] value'.
	
	self assert: result equals: 1
]

{ #category : #'tests-readvariables' }
OCASTBlockTranslatorTest >> testBlockWithTempReturnsLastExpression [
	
	| result |
	result := self testSource: 'example
	^ [ | t | t := 1. t ] value'.
	
	self assert: result equals: 1
]

{ #category : #'tests-arguments' }
OCASTBlockTranslatorTest >> testBlockWithThreeArgumentReturnsLastExpression [
	
	| result |
	result := self testSource: 'example
	^ [ :arg1 :arg2 :arg3 | arg1 + arg2 + arg3 ] value: 2 value: 3 value: 4'.
	
	self assert: result equals: 9
]

{ #category : #'tests-arguments' }
OCASTBlockTranslatorTest >> testBlockWithTwoArgumentReturnsLastExpression [
	
	| result |
	result := self testSource: 'example
	^ [ :arg1 :arg2 | arg1 + arg2 ] value: 2 value: 3'.
	
	self assert: result equals: 5
]

{ #category : #'tests-writevariables' }
OCASTBlockTranslatorTest >> testBlockWritesInstanceVariable [
	
	self testSource: 'example
	[ iVar := 1 ] value'.
	
	self assert: instance iVar equals: 1
]

{ #category : #'tests-writevariables' }
OCASTBlockTranslatorTest >> testBlocksShareOuterVariables [
	
	| result |
	result := self testSource: 'example
	| blocks |
	blocks := [| t |
		{
			"setter" [ :value | t := value ].
			"getter" [ t ]
		}
	] value.
	blocks first value: 5.
	^ blocks second value '.
	
	self assert: result equals: 5
]

{ #category : #'tests-literals' }
OCASTBlockTranslatorTest >> testEmptyBlockReturnsNil [
	
	| result |
	result := self testSource: 'example
	^ [] value'.
	
	self assert: result isNil
]

{ #category : #'tests-readvariables' }
OCASTBlockTranslatorTest >> testNestedBlockWithExternalTempReturnsLastExpression [
	
	| result |
	result := self testSource: 'example
	| t1 t2 |
	t1 := 1.
	t2 := 2.
	^ [ [ t1 ] value + t2 ] value'.
	
	self assert: result equals: 3
]

{ #category : #'tests-writevariables' }
OCASTBlockTranslatorTest >> testOuterBlockTemporaryVariable [
	
	| result |
	result := self testSource: 'example
	| block |
	block := [| t |
		t := 0.
		[ t := t + 1. t ]
	] value.
	^ block value; value '.
	
	self assert: result equals: 2
]

{ #category : #'tests-writevariables' }
OCASTBlockTranslatorTest >> testWritesTemporaryVariable [
	
	| result |
	result := self testSource: 'example
	| t |
	[ t := 1 ] value.
	^ t'.
	
	self assert: result equals: 1
]
