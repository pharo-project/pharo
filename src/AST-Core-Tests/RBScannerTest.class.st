"
SUnit tests for RBScanner
"
Class {
	#name : #RBScannerTest,
	#superclass : #TestCase,
	#category : #'AST-Core-Tests-Parser'
}

{ #category : #initialize }
RBScannerTest >> buildScannerForText: source [
	^RBScanner on: (ReadStream on: source)
]

{ #category : #tests }
RBScannerTest >> testNextLiteralCharacterWithAMissingCharacter [
	| source scanner scannedToken |
	source := '$'.
	scanner := self buildScannerForText: source.
	scannedToken := scanner next.
	self verifyErrorToken: scannedToken message: 'A Character was expected' translated valueExpected: ''

]

{ #category : #tests }
RBScannerTest >> testNextWithAWrongSymbolGetError [
	| source scanner scannedToken |
	source := '#^'.
	scanner := self buildScannerForText: source.
	scannedToken := scanner next.
	self verifyErrorToken: scannedToken message: 'Expecting a literal type' translated valueExpected: '^'
]

{ #category : #tests }
RBScannerTest >> testNextWithAnIdentifierTokenGetTheIdentifierToken [
	| source scanner token |	
	source := 'identifierToken'.
	scanner := self buildScannerForText: source.
	token := scanner next.
	self assert: token isIdentifier.
	self deny: token isError.
	self assert: token value equals: source
]

{ #category : #tests }
RBScannerTest >> testNextWithAnOpeningCommentAtEndGetError [
	| source |	
	source := 'self "'.
	self should: [(self buildScannerForText: source) next] raise: SyntaxErrorNotification
]

{ #category : #tests }
RBScannerTest >> testNextWithAnOpeningCommentGetError [
	| source |	
	source := '"only the opening'.
	self should: [(self buildScannerForText: source) next] raise: SyntaxErrorNotification
]

{ #category : #tests }
RBScannerTest >> testNextWithAnOpeningStringsGetError [
	| source scannedToken |
	source := '''only the opening'.
	scannedToken := (self buildScannerForText: source) next.
	self verifyErrorToken: scannedToken message: 'Unmatched '' in string literal.' translated valueExpected: 'only the opening'
]

{ #category : #tests }
RBScannerTest >> testNextWithAnUnknownCharacterGetError [
	| source scanner scannedToken |	
	source := '¿'.
	scanner := self buildScannerForText: source.
	scannedToken := scanner next.
	self verifyErrorToken: scannedToken message: 'Unknown character' translated valueExpected: source
]

{ #category : #tests }
RBScannerTest >> testNextWithTwoDoubleQuotesInComment [
	| source token |
	source := '"only the"" opening"'.
	self
		shouldnt: [ token := (self buildScannerForText: source) next ]
		raise: SyntaxErrorNotification.
	self assert: token comments first equals: (1 to: source size)
]

{ #category : #tests }
RBScannerTest >> testNextWithTwoDoubleQuotesInCommentGetError [
	| source |
	source := '"only the"" opening'.
	self
		should: [ (self buildScannerForText: source) next ]
		raise: SyntaxErrorNotification
]

{ #category : #tests }
RBScannerTest >> testScanTokens [
	| inp exp |
	inp := 'Object subclass: #NameOfSubclass'.
	exp := {'Object'.
	'subclass:'.
	#NameOfSubclass asString}.
	self assert: (RBScanner scanTokens: inp) equals: exp.
	inp := 'classVariableNames: '''' "ha ha"
package: ''UndefinedClasses-Experiment'.
	exp := {'classVariableNames:' . '' . 'package:' . 'UndefinedClasses-Experiment'}.
	self assert: (RBScanner scanTokens: inp) equals: exp
]

{ #category : #utilities }
RBScannerTest >> verifyErrorToken: scannedToken message: message valueExpected: valueExpected [
	self assert: scannedToken isError.
	self assert: scannedToken cause equals: message.
	self assert: scannedToken value equals: valueExpected.
	self assert: scannedToken start equals: 1.
]
