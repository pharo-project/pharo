Class {
	#name : #RBGlobalVariableRuleTest,
	#superclass : #TestCase,
	#category : #'GeneralRules-Tests'
}

{ #category : #tests }
RBGlobalVariableRuleTest >> applyingCritiques [
	^ self class methods
		select: [ :m | (RBGlobalVariablesUsage new check: m) isNotEmpty ]
]

{ #category : #tests }
RBGlobalVariableRuleTest >> methodNotUsesGlobal [
	String streamContents: [ :str | str nextPut: #something ]
]

{ #category : #tests }
RBGlobalVariableRuleTest >> methodNotUsesGlobal2 [
	| x y z a b c |
	x := '...'.
	y := 1.9 .
	z := #(a b c).
	a := { 1 . 2 . 3 }.
	b := #a.
	c := $a.
	
	^ x
]

{ #category : #tests }
RBGlobalVariableRuleTest >> methodUsesGlobal [
	Smalltalk version 
]

{ #category : #tests }
RBGlobalVariableRuleTest >> methodUsesGlobal2 [
	Processor activeProcess
]

{ #category : #tests }
RBGlobalVariableRuleTest >> testTheRuleCanCheckMethodsWithDifferentKindsOfLiterals [
	self assertEmpty: (RBGlobalVariablesUsage new check: self class >> #methodNotUsesGlobal2)
]

{ #category : #tests }
RBGlobalVariableRuleTest >> testTheRuleOnlyDetect2MethodsWithGlobalVariables [
	self assert: self applyingCritiques size equals: 2
]

{ #category : #tests }
RBGlobalVariableRuleTest >> testTheRuleOnlyMethodUsesGlobal1And2 [
	self assertCollection: (self applyingCritiques collect: #selector) hasSameElements: {#methodUsesGlobal . #methodUsesGlobal2}
]
