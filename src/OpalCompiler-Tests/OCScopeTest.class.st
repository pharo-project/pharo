Class {
	#name : 'OCScopeTest',
	#superclass : 'TestCase',
	#instVars : [
		'ivarForTesting'
	],
	#classVars : [
		'MyClassVarForTesting'
	],
	#category : 'OpalCompiler-Tests-Semantic',
	#package : 'OpalCompiler-Tests',
	#tag : 'Semantic'
}

{ #category : 'accessing' }
OCScopeTest class >> myClassVarForTesting [

	^MyClassVarForTesting
]

{ #category : 'accessing' }
OCScopeTest >> ivarForTesting [

	^ ivarForTesting
]

{ #category : 'tests - scope from contexts' }
OCScopeTest >> testASTContextFromACleanBlockShouldNotExposeOuterVariables [

	| myVariable ctx |
	myVariable := 27.
	
	ctx := [ Object new ] asContext.
	
	self assert: (ctx astScope lookupVar: 'myVariable') isNil.
]

{ #category : 'tests - scope from contexts' }
OCScopeTest >> testASTContextFromANonCleanBlockShouldExposeOuterVariables [

	| myVariable ctx |
	myVariable := 27.
	
	ctx := [ Object new. self ] asContext.
	
	self assert: (ctx astScope lookupVar: 'myVariable') isNotNil.
]
