"
SUnit tests for RBScanner
"
Class {
	#name : #RBScannerTest,
	#superclass : #TestCase,
	#category : #'AST-Tests-Core'
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
	| source scanner token |
	source := '"only the"" opening"'.
	self
		shouldnt: [ token := (scanner := self buildScannerForText: source) next ]
		raise: SyntaxErrorNotification.
	self assert: token comments first equals: (1 to: source size)
]

{ #category : #tests }
RBScannerTest >> testNextWithTwoDoubleQuotesInCommentGetError [
	| source scanner token |
	source := '"only the"" opening'.
	self
		should: [ token := (scanner := self buildScannerForText: source) next ]
		raise: SyntaxErrorNotification
]

{ #category : #utilities }
RBScannerTest >> verifyErrorToken: scannedToken message: message valueExpected: valueExpected [
	self assert: scannedToken isError.
	self assert: scannedToken cause equals: message.
	self assert: scannedToken value equals: valueExpected.
	self assert: scannedToken start equals: 1.
]
