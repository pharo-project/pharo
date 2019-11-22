"
SUnit tests for self evaluting objects 
"
Class {
	#name : #SelfEvaluatingObjectTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-WithCompiler'
}

{ #category : #utilities }
SelfEvaluatingObjectTest >> assertCode: code print: aString [
	self assert: (self class evaluate: code) printString equals: aString
]

{ #category : #tests }
SelfEvaluatingObjectTest >> testArray [

	self assertCode: '#(1 2 3)' print: '#(1 2 3)'.
	self assertCode: '{1 . 2 . 3}' print: '#(1 2 3)'.
	self assertCode: '{1 + 0 . 2 . 3}' print: '#(1 2 3)'.
	self assertCode: '{1 + 0 . 1 @ 2 . 3}' print: '{1. (1@2). 3}'.
	self assertCode: '{2@3}' print: '{(2@3)}'.
	self assertCode: '{Object new}' print:  'an Array(an Object)'.
	self assertCode: '{Rectangle new . Object new}' print:  'an Array(nil corner: nil an Object)'.
	self assertCode: '{10@10 corner: 20@20 . 100@100 corner: 200@200}' print: '{(10@10) corner: (20@20). (100@100) corner: (200@200)}'
]

{ #category : #tests }
SelfEvaluatingObjectTest >> testObjects [

	self assert: 10 isSelfEvaluating.
	self assert: $a isSelfEvaluating.
	self assert: 3.14157 isSelfEvaluating.
	self assert: #(1 2 3) isSelfEvaluating.
	self assert: #abc isSelfEvaluating.
	self assert: 'abc' isSelfEvaluating.

	self assert: Object isSelfEvaluating.
	self assert: Object new isSelfEvaluating not.
	
	self assert: {Float infinity . Float nan. Float infinity negated} isSelfEvaluating.

	self assert: (Array with: 10) isSelfEvaluating.
	self assert: (Array with: Object new) isSelfEvaluating not.

	self assert: true isSelfEvaluating.
	self assert: false isSelfEvaluating.

	self assert: nil isSelfEvaluating.

	self assert: (1 to: 10) isSelfEvaluating.
	self assert: (1->2) isSelfEvaluating.
	self assert: Color red isSelfEvaluating.

]
