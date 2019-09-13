Class {
	#name : #OCASTSpecialLiteralTranslatorTest,
	#superclass : #OCASTTranslatorTest,
	#category : #'OpalCompiler-Tests-AST'
}

{ #category : #'testing - literals' }
OCASTSpecialLiteralTranslatorTest >> testDynamicLiteralArrayIsNotInLiteralFrame [
	
	| aCompiledMethod array |
	aCompiledMethod := self compileExample: #examplePushDynamicLiteralArray.
	array := self executeMethodOnInstance: aCompiledMethod.
	
	self deny: (aCompiledMethod literals identityIncludes: array).
]

{ #category : #'testing - literals' }
OCASTSpecialLiteralTranslatorTest >> testDynamicLiteralArrayReturnsAlwaysNewArray [
	
	| aCompiledMethod |
	aCompiledMethod := self compileExample: #examplePushDynamicLiteralArray.
	
	self assert: (self executeMethodOnInstance: aCompiledMethod) ~~ (self executeMethodOnInstance: aCompiledMethod)
]

{ #category : #'testing - literals' }
OCASTSpecialLiteralTranslatorTest >> testDynamicLiteralArrayReturnsArray [

	self assert: (self testExample: #examplePushDynamicLiteralArray) equals: #(1 3)
]

{ #category : #'testing - literals' }
OCASTSpecialLiteralTranslatorTest >> testDynamicLiteralArrayReturnsEqualsArray [
	
	| aCompiledMethod |
	aCompiledMethod := self compileExample: #examplePushDynamicLiteralArray.
	
	self assert: (self executeMethodOnInstance: aCompiledMethod) equals: (self executeMethodOnInstance: aCompiledMethod)
]

{ #category : #'testing - literals' }
OCASTSpecialLiteralTranslatorTest >> testSpecialLiteralsNotInLiteralFrame [

	self testExample: #examplePushNonSpecialSmallInteger.
	self assert: instance result equals: 10.
	self assert: (method hasLiteral: 10)
	
]
