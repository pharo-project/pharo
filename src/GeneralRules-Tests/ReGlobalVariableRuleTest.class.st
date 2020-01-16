Class {
	#name : #ReGlobalVariableRuleTest,
	#superclass : #TestCase,
	#category : #'GeneralRules-Tests-Migrated'
}

{ #category : #tests }
ReGlobalVariableRuleTest >> applyingCritiques [
	^ self class methods
		select: [ :m | (ReGlobalVariablesUsage new check: m) isNotEmpty ]
]

{ #category : #tests }
ReGlobalVariableRuleTest >> methodNotUsesGlobal [
	String streamContents: [ :str | str nextPut: #something ]
]

{ #category : #tests }
ReGlobalVariableRuleTest >> methodNotUsesGlobal2 [
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
ReGlobalVariableRuleTest >> methodUsesGlobal [
	Smalltalk version 
]

{ #category : #tests }
ReGlobalVariableRuleTest >> methodUsesGlobal2 [
	Processor activeProcess
]

{ #category : #tests }
ReGlobalVariableRuleTest >> testTheRuleCanCheckMethodsWithDifferentKindsOfLiterals [
	self assertEmpty: (ReGlobalVariablesUsage new check: self class >> #methodNotUsesGlobal2)
]

{ #category : #tests }
ReGlobalVariableRuleTest >> testTheRuleOnlyDetect2MethodsWithGlobalVariables [
	self assert: self applyingCritiques size equals: 2
]

{ #category : #tests }
ReGlobalVariableRuleTest >> testTheRuleOnlyMethodUsesGlobal1And2 [
	self assertCollection: (self applyingCritiques collect: #selector) hasSameElements: {#methodUsesGlobal . #methodUsesGlobal2}
]
