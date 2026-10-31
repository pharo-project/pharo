"
Tests for the baseline provider chain used by RubGutterDiffDisplayer.
"
Class {
	#name : 'RubMethodBaselineProviderTest',
	#superclass : 'TestCase',
	#category : 'Rubric-Tests-Editing-Widgets',
	#package : 'Rubric-Tests',
	#tag : 'Editing-Widgets'
}

{ #category : 'as yet unclassified' }
RubMethodBaselineProviderTest >> foo [ ^ 42
]

{ #category : 'tests' }
RubMethodBaselineProviderTest >> testCompiledMethodProvider [

	| provider method |

	provider := RubCompiledMethodBaselineProvider new.
	method := RubMethodBaselineProviderTest >> #testCompiledMethodProvider.
	self assert: (provider baselineForMethod: method) equals: method sourceCode
]

{ #category : 'tests' }
RubMethodBaselineProviderTest >> testFallbackChainOnFirstProvider [
	| provider method result |
	provider := RubCompiledMethodBaselineProvider new.
	provider fallbackProvider: RubCompiledMethodBaselineProvider new.
	method := RubMethodBaselineProviderTest >> #testFallbackChainOnFirstProvider.
	result := provider baselineForMethod: method ifNone: [ 'fallback' ].
	self assert: result equals: method sourceCode
]

{ #category : 'tests' }
RubMethodBaselineProviderTest >> testFallbackChainOnSecond [
	| first second method result |
	first := RubGitMethodBaselineProvider new.
	second := RubCompiledMethodBaselineProvider new.
	first fallbackProvider: second.
	method := RubMethodBaselineProviderTest >> #testFallbackChainOnSecond.
	result := first baselineForMethod: method ifNone: [ 'fallback' ].
	self assert: result equals: method sourceCode
]

{ #category : 'tests' }
RubMethodBaselineProviderTest >> testFallbackChainWithNilReturn [
	| first second method result |
	first := RubGitMethodBaselineProvider new.
	second := RubCompiledMethodBaselineProvider new.
	first fallbackProvider: second.
	method := RubMethodBaselineProviderTest >> #testFallbackChainWithNilReturn.
	result := first baselineForMethod: method ifNone: [ 'default' ].
	self assert: result equals: method sourceCode
]

{ #category : 'tests' }
RubMethodBaselineProviderTest >> testGitProviderReturnsNilWhenNoIceberg [
	| provider method |
	
	provider := RubGitMethodBaselineProvider new.
	self class compile: 'dynamicMethod ^ 42'.
	method :=  self class >> #dynamicMethod.
	self assert: (provider baselineForMethod: method) isNil.
	self class removeSelector: #dynamicMethod.
]
