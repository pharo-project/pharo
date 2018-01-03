"
SUnit tests for self evaluting objects 
"
Class {
	#name : #SelfEvaluatingObjectTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Objects'
}

{ #category : #utilities }
SelfEvaluatingObjectTest >> assertCode: code print: aString [
	self assert: (self class evaluate: code) printString = aString
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
