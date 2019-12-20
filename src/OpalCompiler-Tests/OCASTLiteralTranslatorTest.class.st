Class {
	#name : #OCASTLiteralTranslatorTest,
	#superclass : #OCASTTranslatorTest,
	#instVars : [
		'literalExample',
		'expectedLiteralValue'
	],
	#category : #'OpalCompiler-Tests-AST'
}

{ #category : #'building suites' }
OCASTLiteralTranslatorTest class >> testParameters [

	^ super testParameters * {
			{ #literalExample -> #exampleNonSpecialLiteralInteger. #expectedLiteralValue -> 10 }.
			{ #literalExample -> #exampleLiteralLargeInteger. #expectedLiteralValue -> 16rFFFFFFFFFFFFFFFF }.
			{ #literalExample -> #exampleLiteralCharacter. #expectedLiteralValue -> $H }.
			{ #literalExample -> #exampleLiteralArray. #expectedLiteralValue -> #(1 2 3) }.
			{ #literalExample -> #exampleLiteralByteArray. #expectedLiteralValue -> #[1 2 3] }.
			{ #literalExample -> #exampleLiteralFloat. #expectedLiteralValue -> 1.2 }.
			{ #literalExample -> #exampleLiteralBoxedFloat. #expectedLiteralValue -> 9.999999999999996e157 }.
			{ #literalExample -> #exampleLiteralByteSymbol. #expectedLiteralValue -> #bytesymbol }.
			{ #literalExample -> #exampleLiteralWideSymbol. #expectedLiteralValue -> #'áèîöÑü' }.
			{ #literalExample -> #exampleLiteralByteString. #expectedLiteralValue -> 'bytestring' }.
			{ #literalExample -> #exampleLiteralWideString. #expectedLiteralValue -> 'áèîöÑü' }
	}
]

{ #category : #accessing }
OCASTLiteralTranslatorTest >> expectedLiteralValue [
	^ expectedLiteralValue
]

{ #category : #accessing }
OCASTLiteralTranslatorTest >> expectedLiteralValue: anObject [
	expectedLiteralValue := anObject
]

{ #category : #accessing }
OCASTLiteralTranslatorTest >> literalExample [
	^ literalExample
]

{ #category : #accessing }
OCASTLiteralTranslatorTest >> literalExample: anObject [
	literalExample := anObject
]

{ #category : #'testing - literals' }
OCASTLiteralTranslatorTest >> testLiteralIsInLiteralFrame [
	
	| array |
	array := self testExample: literalExample.
	
	self assert: (method literals identityIncludes: array).
]

{ #category : #'testing - literals' }
OCASTLiteralTranslatorTest >> testLiteralReturnsAlwaysSameLiteral [
	self compileExample: literalExample.

	self assert: (self executeMethodOnInstance: method) identicalTo: (self executeMethodOnInstance: method)
]

{ #category : #'testing - literals' }
OCASTLiteralTranslatorTest >> testLiteralReturnsLiteral [
	
	self assert: (self testExample: literalExample) equals: expectedLiteralValue
]
