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
	Object new 
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
RBGlobalVariableRuleTest >> testTheRuleOnlyDetect2MethodsWithGlobalVariables [
	self assert: self applyingCritiques size equals: 2
]

{ #category : #tests }
RBGlobalVariableRuleTest >> testTheRuleOnlyMethodUsesGlobal1And2 [
	self
		assert: (self applyingCritiques collect: #selector) asArray
		equals: {#methodUsesGlobal . #methodUsesGlobal2}
]
