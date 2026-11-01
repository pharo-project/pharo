"
I test `ReDoNotUseAsPointOnLiteralNumberRule`: it reports `asPoint` sent to a literal number and rewrites it into a point built with `@` (e.g. `33 asPoint` becomes `33 @ 33`), and it ignores any other receiver.
"
Class {
	#name : 'ReDoNotUseAsPointOnLiteralNumberRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Coding Idiom Violation',
	#package : 'General-Rules-Tests',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'helpers' }
ReDoNotUseAsPointOnLiteralNumberRuleTest >> applyCritiqueOn: aSourceString [
	"Compile aSourceString as #method, check that the rule reports exactly one critique,
	apply its change and answer the AST of the new #method."

	| critiques |
	self generateMethod: aSourceString.
	critiques := self engineCritiquesOnMethod.
	self assert: critiques size equals: 1.
	self assert: critiques anyOne providesChange.
	critiques anyOne change execute.
	^ (self class >> #method) ast
]

{ #category : 'helpers' }
ReDoNotUseAsPointOnLiteralNumberRuleTest >> assertRewriteOf: aSourceString is: anExpectedSourceString [
	"The change regenerates the source with the formatter, so compare ASTs rather than strings."

	self assert: (self applyCritiqueOn: aSourceString) equals: (OCParser parseMethod: anExpectedSourceString)
]

{ #category : 'helpers' }
ReDoNotUseAsPointOnLiteralNumberRuleTest >> engineCritiquesOnMethod [
	"Answer the critiques of the rule on #method, computed by the critic engine.
	Unlike #myCritiquesOnMethod:, which runs the rule on a copy of the AST, the engine anchors
	the critiques on the compiled method, so their change can be applied."

	^ (ReCriticEngine nodeCritiquesOf: self class >> #method) select: [ :each | each rule class == self subjectUnderTest ]
]

{ #category : 'tests' }
ReDoNotUseAsPointOnLiteralNumberRuleTest >> testCritiqueAnchorsTheMessage [

	| critique |
	self generateMethod: 'method ^ 1 + 33 asPoint'.
	critique := self engineCritiquesOnMethod anyOne.
	self
		assert:
		((self class >> #method) sourceCode copyFrom: critique sourceAnchor interval first to: critique sourceAnchor interval last)
		equals: '33 asPoint'
]

{ #category : 'tests' }
ReDoNotUseAsPointOnLiteralNumberRuleTest >> testNoCritiqueOnExpressionReceiver [

	self generateMethod: 'method ^ (1 + 2) asPoint'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]

{ #category : 'tests' }
ReDoNotUseAsPointOnLiteralNumberRuleTest >> testNoCritiqueOnOtherLiterals [
	"Only literal numbers are rewritten: a literal string, symbol or array is left alone."

	self generateMethod: 'method ^ { ''33'' asPoint. #foo asPoint. #(1 2) asPoint }'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]

{ #category : 'tests' }
ReDoNotUseAsPointOnLiteralNumberRuleTest >> testNoCritiqueOnOtherMessages [

	self generateMethod: 'method ^ 33 asString'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]

{ #category : 'tests' }
ReDoNotUseAsPointOnLiteralNumberRuleTest >> testNoCritiqueOnVariableReceiver [

	self generateMethod: 'method | n | n := 3. ^ n asPoint'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]

{ #category : 'tests' }
ReDoNotUseAsPointOnLiteralNumberRuleTest >> testRewriteKeepsFormattingOfTheMethod [

	self applyCritiqueOn: 'method
	"comment"
	^ Array
		with: 33 asPoint
		with: 2'.
	self assert: (self class >> #method) sourceCode equals: 'method
	"comment"
	^ Array
		with: 33 @ 33
		with: 2'
]

{ #category : 'tests' }
ReDoNotUseAsPointOnLiteralNumberRuleTest >> testRewriteKeepsPrecedence [
	"The new point is put in parentheses when a unary message is sent to it."

	self assertRewriteOf: 'method ^ 33 asPoint x' is: 'method ^ (33 @ 33) x'.
	self assert: (self perform: #method) equals: 33
]

{ #category : 'tests' }
ReDoNotUseAsPointOnLiteralNumberRuleTest >> testRewriteKeepsPrecedenceAsArgument [
	"The new point is put in parentheses when it is the argument of a binary message."

	self assertRewriteOf: 'method ^ 1 + 33 asPoint' is: 'method ^ 1 + (33 @ 33)'.
	self assert: (self perform: #method) equals: 34 @ 34
]

{ #category : 'tests' }
ReDoNotUseAsPointOnLiteralNumberRuleTest >> testRewritesFloatLiteral [

	self assertRewriteOf: 'method ^ 1.5 asPoint' is: 'method ^ 1.5 @ 1.5'.
	self assert: (self perform: #method) equals: 1.5 @ 1.5
]

{ #category : 'tests' }
ReDoNotUseAsPointOnLiteralNumberRuleTest >> testRewritesIntegerLiteral [

	self assertRewriteOf: 'method ^ 33 asPoint' is: 'method ^ 33 @ 33'.
	self assert: (self perform: #method) equals: 33 @ 33.
	self assertEmpty: self engineCritiquesOnMethod
]

{ #category : 'tests' }
ReDoNotUseAsPointOnLiteralNumberRuleTest >> testRewritesNegativeLiteral [

	self assertRewriteOf: 'method ^ -3 asPoint' is: 'method ^ -3 @ -3'.
	self assert: (self perform: #method) equals: -3 @ -3
]

{ #category : 'tests' }
ReDoNotUseAsPointOnLiteralNumberRuleTest >> testRewritesOnlyTheLiteralReceiver [

	self assertRewriteOf: 'method | p | p := 2 @ 3. ^ 10 asPoint + p asPoint' is: 'method | p | p := 2 @ 3. ^ (10 @ 10) + p asPoint'.
	self assert: (self perform: #method) equals: 12 @ 13
]
