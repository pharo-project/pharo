"
SUnit tests for RBScanner
"
Class {
	#name : 'ASTScannerTest',
	#superclass : 'TestCase',
	#category : 'AST-Core-Tests-Parser',
	#package : 'AST-Core-Tests',
	#tag : 'Parser'
}

{ #category : 'initialize' }
ASTScannerTest >> buildScannerForText: source [
	^ self scannerClass on: source readStream
]

{ #category : 'initialize' }
ASTScannerTest >> scannerClass [
	^ RBScanner
]

{ #category : 'tests - Comment' }
ASTScannerTest >> testCommentTokenHasTheRightValue [
	| scanner token |
	scanner := self buildScannerForText: String tab, ' "simple comment" firstToken'.
	token := scanner next comments first.
	self assert: token value equals: 'simple comment'.

	scanner := self buildScannerForText: String tab, ' """nested at beggining"" comment" firstToken'.
	token := scanner next comments first.
	self assert: token value equals: '"nested at beggining" comment'.

	scanner := self buildScannerForText: String tab, ' "comment ""nested at end""" firstToken'.
	token := scanner next comments first.
	self assert: token value equals: 'comment "nested at end"'
]

{ #category : 'tests - Comment' }
ASTScannerTest >> testCommentTokenStartAtFirstQuote [
	| scanner token str comment |
	str := ' "simple comment" firstToken'.
	comment := '"simple comment"'.
	scanner := self buildScannerForText: str.
	token := scanner next comments first.
	self assert: token isComment.
	self assert: token start equals: (str findString: comment).

	str := ' """nested at beggining"" comment" firstToken'.
	comment := '"""nested at beggining"" comment"'.
	scanner := self buildScannerForText: str .
	token := scanner next comments first.
	self assert: token isComment.
	self assert: token start equals: (str findString: comment).

	str := '"comment ""nested at end""" firstToken'.
	comment := '"comment ""nested at end"""'.
	scanner := self buildScannerForText: str.
	token := scanner next comments first.
	self assert: token isComment.
	self assert: token start equals: (str findString: comment)
]

{ #category : 'tests - Comment' }
ASTScannerTest >> testCommentTokenStopAtLastQuote [
	| scanner token str comment |
	str := ' "simple comment" firstToken'.
	comment := '"simple comment"'.
	scanner := self buildScannerForText: str.
	token := scanner next comments first.
	self assert: token isComment.
	self assert: token stop equals: (str findString: comment) + comment size - 1.

	str := ' """nested at beggining"" comment" firstToken'.
	comment := '"""nested at beggining"" comment"'.
	scanner := self buildScannerForText: str .
	token := scanner next comments first.
	self assert: token isComment.
	self assert: token stop equals: (str findString: comment) + comment size - 1.

	str := '"comment ""nested at end""" firstToken'.
	comment := '"comment ""nested at end"""'.
	scanner := self buildScannerForText: str.
	token := scanner next comments first.
	self assert: token isComment.
	self assert: token stop equals: (str findString: comment) + comment size - 1
]

{ #category : 'tests - Comment' }
ASTScannerTest >> testMultipleCommentTokensAreWellSeparated [
	| scanner token comments |
	scanner := self buildScannerForText: String tab, ' "first comment" " second comment" firstToken'.
	comments := scanner next comments.
	token := comments first.
	self assert: token isComment.
	self assert: token value equals: 'first comment'.

	token := comments second.
	self assert: token isComment.
	self assert: token value equals: ' second comment'
]

{ #category : 'tests - Literal' }
ASTScannerTest >> testNextAcceptAllAlphabeticAsLiteral [
	| scanner token |
	scanner := self buildScannerForText: '#alpha'.
	token := scanner next.
	self assert: token isLiteralToken
]

{ #category : 'tests - NumberLiteral' }
ASTScannerTest >> testNextAcceptFloatNumbers [

	| scanner token |
	scanner := self buildScannerForText: '3.2'.
	token := scanner next.
	self assert: token isNumberLiteralToken
]

{ #category : 'tests - SpecialCharacters' }
ASTScannerTest >> testNextAcceptLiteralArrayWithBracket [
	"Note: RBScanner only spots the beginning of the array."
	| scanner token |

	scanner := self buildScannerForText: '#['.
	token := scanner next.
	self assert: token isLiteralArrayToken
]

{ #category : 'tests - SpecialCharacters' }
ASTScannerTest >> testNextAcceptLiteralArrayWithParenthesis [
	"Note: RBScanner only spots the beginning of the array."
	| scanner token |
	scanner := self buildScannerForText: '#('.
	token := scanner next.
	self assert: token isLiteralArrayToken
]

{ #category : 'tests - NumberLiteral' }
ASTScannerTest >> testNextAcceptRadixNumbers [

	| scanner token |
	scanner := self buildScannerForText: '10r10'.
	token := scanner next.
	self assert: token isNumberLiteralToken.

	scanner := self buildScannerForText: '2r10'.
	token := scanner next.
	self assert: token isNumberLiteralToken.

	scanner := self buildScannerForText: '16rfa'.
	token := scanner next.
	self assert: token isNumberLiteralToken.

	scanner := self buildScannerForText: '400r380'.
	token := scanner next.
	self assert: token isNumberLiteralToken
]

{ #category : 'tests - Keyword' }
ASTScannerTest >> testNextAllAlphabeticEndingWithColonGiveKeyword [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken:'.
	token := scanner next.
	self assert: token isKeyword
]

{ #category : 'tests - Identifiers' }
ASTScannerTest >> testNextAllAlphabeticsGiveIdentifier [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken'.
	token := scanner next.
	self assert: token isIdentifier
]

{ #category : 'tests - Keyword' }
ASTScannerTest >> testNextAlphabeticAndDigitEndingWithColonGiveKeyword [
	| scanner token |
	scanner := self buildScannerForText: 'first123Token:'.
	token := scanner next.
	self assert: token isKeyword
]

{ #category : 'tests - Identifiers' }
ASTScannerTest >> testNextAlphabeticsAndDigitGiveIdentifier [
	| scanner token |
	scanner := self buildScannerForText: 'first123Token'.
	token := scanner next.
	self assert: token isIdentifier
]

{ #category : 'tests - Start' }
ASTScannerTest >> testNextAssignementStartAtColon [
	| scanner token |
	scanner := self buildScannerForText: ':='.
	token := scanner next.
	self assert: token start equals: 1
]

{ #category : 'tests - Stop' }
ASTScannerTest >> testNextAssignementStopAtEqual [
	| scanner token |
	scanner := self buildScannerForText: ':='.
	token := scanner next.
	self assert: token stop equals: 2
]

{ #category : 'tests - Error' }
ASTScannerTest >> testNextBaseOfRadixNumbersCanNotBeOne [

	| scanner token |
	scanner := self buildScannerForText: '1r1a'.
	token := scanner next.
	self assert: token isError.
	self assert: token start equals: 1
]

{ #category : 'tests - NumberLiteral' }
ASTScannerTest >> testNextBeginningWithDigitIsNumber [

	| scanner token |
	scanner := self buildScannerForText: '1'.
	token := scanner next.
	self assert: token isNumberLiteralToken.

	scanner := self buildScannerForText: '1a'.
	token := scanner next.
	self assert: token isNumberLiteralToken.

	scanner := self buildScannerForText: '1='.
	token := scanner next.
	self assert: token isNumberLiteralToken.

	scanner := self buildScannerForText: '1$'.
	token := scanner next.
	self assert: token isNumberLiteralToken.

	scanner := self buildScannerForText: '123'.
	token := scanner next.
	self assert: token isNumberLiteralToken
]

{ #category : 'tests - BinarySelector' }
ASTScannerTest >> testNextBinaryGivesBinarySelector [
	| scanner token |
	scanner := self buildScannerForText: '+'.
	token := scanner next.
	self assert: token isBinary
]

{ #category : 'tests - BinarySelector' }
ASTScannerTest >> testNextBinarySelectorCanBeMultipleBinaries [
	| scanner token |
	scanner := self buildScannerForText: '+-=/'.
	token := scanner next.
	self assert: token isBinary.
	self assert: token value equals: '+-=/'
]

{ #category : 'tests - Start' }
ASTScannerTest >> testNextBinarySelectorStartAtFirstLetter [
	| scanner token |
	scanner := self buildScannerForText: '+-=/'.
	token := scanner next.
	self assert: token value equals: '+-=/'.
	self assert: token start equals: 1
]

{ #category : 'tests - Stop' }
ASTScannerTest >> testNextBinarySelectorStopAtLastLetter [
	| scanner token |
	scanner := self buildScannerForText: '+-=/'.
	token := scanner next.
	self assert: token value equals: '+-=/'.
	self assert: token stop equals: 4
]

{ #category : 'tests - BinarySelector' }
ASTScannerTest >> testNextBinarySelectorStopsAtNonBinary [
	| scanner token |
	scanner := self buildScannerForText: '+a'.
	token := scanner next.
	self assert: token isBinary.
	self assert: token value equals: '+'.

	scanner := self buildScannerForText: '+1'.
	token := scanner next.
	self assert: token isBinary.
	self assert: token value equals: '+'.

	scanner := self buildScannerForText: '+('.
	token := scanner next.
	self assert: token isBinary.
	self assert: token value equals: '+'.

	scanner := self buildScannerForText: '+`'.
	token := scanner next.
	self assert: token isBinary.
	self assert: token value equals: '+'
]

{ #category : 'tests - BinarySelector' }
ASTScannerTest >> testNextBinarySelectorValueIsBinary [
	| scanner token |
	scanner := self buildScannerForText: '+'.
	token := scanner next.
	self assert: token value equals: '+'
]

{ #category : 'tests - Comment' }
ASTScannerTest >> testNextCommentIsCommentToken [
	| scanner token |
	scanner := self buildScannerForText: String tab, ' "simple comment" firstToken'.
	token := scanner next comments first.
	self assert: token isComment.

	scanner := self buildScannerForText: String tab, ' """nested at beggining"" comment" firstToken'.
	token := scanner next comments first.
	self assert: token isComment.

	scanner := self buildScannerForText: String tab, ' "comment ""nested at end""" firstToken'.
	token := scanner next comments first.
	self assert: token isComment
]

{ #category : 'tests - Separators' }
ASTScannerTest >> testNextDoesNotAddLineBreakToComments [
	| scanner token |
	scanner := self buildScannerForText: '
firstToken'.
	token := scanner next.
	self assert: token comments isNil
]

{ #category : 'tests - Separators' }
ASTScannerTest >> testNextDoesNotAddSpacesToComments [
	| scanner token |
	scanner := self buildScannerForText: String space, String space,'firstToken'.
	token := scanner next.
	self assert: token comments isNil
]

{ #category : 'tests - Separators' }
ASTScannerTest >> testNextDoesNotAddTabToComments [
	| scanner token |
	scanner := self buildScannerForText: String tab, 'firstToken'.
	token := scanner next.
	self assert: token comments isNil
]

{ #category : 'tests - Error' }
ASTScannerTest >> testNextDollarSignErrorHasDollarValue [
	| scanner token |
	scanner := self buildScannerForText: '$'.
	token := scanner next.
	self assert: token value equals: '$'
]

{ #category : 'tests - Error' }
ASTScannerTest >> testNextDollarSignExpectedACharacter [
	| scanner token |
	scanner := self buildScannerForText: '$'.
	token := scanner next.
	self assert: token cause equals: 'Character expected'
]

{ #category : 'tests - Error' }
ASTScannerTest >> testNextDollarSignIsError [
	| scanner token |
	scanner := self buildScannerForText: '$'.
	token := scanner next.
	self assert: token isError
]

{ #category : 'tests - Separators' }
ASTScannerTest >> testNextEOFContainsCommentsWhenNoOtherToken [
	| scanner token |
	scanner := self buildScannerForText: '"start comment"'.

	token := scanner next.
	self assert: token isEOF.
	self assert: token comments size equals: 1.
	self assert: token start equals: 16
]

{ #category : 'tests - Start' }
ASTScannerTest >> testNextErrorStartAtFirstCharacter [
	| scanner token |
	scanner := self buildScannerForText: '$'.
	token := scanner next.
	self assert: token start equals: 1.

	scanner := self buildScannerForText: '#123alpha'.
	token := scanner next.
	self assert: token isError.
	self assert: token start equals: 1
]

{ #category : 'tests - Stop' }
ASTScannerTest >> testNextErrorStopAtErrorCharacter [
	| scanner token |
	scanner := self buildScannerForText: '$'.
	token := scanner next.
	self assert: token stop equals: 1.

	scanner := self buildScannerForText: '#123alpha'.
	token := scanner next.
	self assert: token isError.
	self assert: token stop equals: 1
]

{ #category : 'tests - Literal' }
ASTScannerTest >> testNextFalseIsLiteral [
	| scanner token |
	scanner := self buildScannerForText: 'false'.
	token := scanner next.
	self assert: token isLiteralToken.
	self assert: token value equals: false
]

{ #category : 'tests - Separators' }
ASTScannerTest >> testNextFirstTokenContainsFollowingComments [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken "following comment"'.

	"The first token 'firstToken' takes the previous comment and the following one"
	token := scanner next.
	self assert: token comments size equals: 1.
	self assert: token comments first isComment
]

{ #category : 'tests - Separators' }
ASTScannerTest >> testNextFirstTokenContainsPrecedingComments [
	| scanner token |
	scanner := self buildScannerForText: '"start comment"firstToken'.
	token := scanner next.
	self assert: token comments size equals: 1.
	self assert: token comments first isComment
]

{ #category : 'tests - NumberLiteral' }
ASTScannerTest >> testNextFloatNumbersCanBeNegative [

	| scanner token |
	scanner := self buildScannerForText: '-3.2'.
	token := scanner next.
	self assert: token value equals: -3.2
]

{ #category : 'tests - NumberLiteral' }
ASTScannerTest >> testNextFloatNumbersCanNotHaveSpaces [

	| scanner token |
	scanner := self buildScannerForText: '3 .2'.
	token := scanner next.
	self assert: token value equals: 3.

	scanner := self buildScannerForText: '3. 2'.
	token := scanner next.
	self assert: token value equals: 3
]

{ #category : 'tests - NumberLiteral' }
ASTScannerTest >> testNextFloatNumbersHasFloatValue [

	| scanner token |
	scanner := self buildScannerForText: '3.2'.
	token := scanner next.
	self assert: token value equals: 3.2
]

{ #category : 'tests - Error' }
ASTScannerTest >> testNextHashtagCanNotStandAlone [
	| scanner token |
	scanner := self buildScannerForText: '#'.
	token := scanner next.
	self assert: token isError
]

{ #category : 'tests - Literal' }
ASTScannerTest >> testNextHashtagStringIsLiteral [
	| scanner token |
	scanner := self buildScannerForText: '#''the world has changed'''.
	token := scanner next.
	self assert: token isLiteralToken.
	self assert: token value equals: #'the world has changed'
]

{ #category : 'tests - Identifiers' }
ASTScannerTest >> testNextIdentifierCanContainUnderscore [
	| scanner token |
	scanner := self buildScannerForText: 'first_Token'.
	token := scanner next.
	self assert: token isIdentifier.
	self assert: token value equals: 'first_Token'.

	scanner := self buildScannerForText: '_firstToken'.
	token := scanner next.
	self assert: token isIdentifier.
	self assert: token value equals: '_firstToken'.

	scanner := self buildScannerForText: 'firstToken_'.
	token := scanner next.
	self assert: token isIdentifier.
	self assert: token value equals: 'firstToken_'
]

{ #category : 'tests - Identifiers' }
ASTScannerTest >> testNextIdentifierCanNotBeginWithBinary [
	| scanner token |

	"Scanner doesn't recognise + as the beginning of an Identifier."
	scanner := self buildScannerForText: '+firstToken'.
	token := scanner next.
	self assert: token isBinary.
	self assert: token value equals: '+'.

	"Scanner separate them into 2 tokens with the second corresponding to an Identifier."
	token := scanner next.
	self assert: token isIdentifier.
	self assert: token value equals: 'firstToken'
]

{ #category : 'tests - Identifiers' }
ASTScannerTest >> testNextIdentifierCanNotBeginWithDigit [
	| scanner token |
	scanner := self buildScannerForText: '123firstToken'.

	"Scanner doesn't recognise 123 as the beginning of an Identifier."
	token := scanner next.
	self assert: token isNumberLiteralToken.
	self assert: token value equals: 123.

	"Scanner separate them into 2 tokens with the second corresponding to an Identifier."
	token := scanner next.
	self assert: token isIdentifier.
	self assert: token value equals: 'firstToken'
]

{ #category : 'tests - Identifiers' }
ASTScannerTest >> testNextIdentifierCanNotEndWithBinary [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken+'.

	"The first token is still recognised as Identifier but the + insn't taken into account."
	token := scanner next.
	self assert: token isIdentifier.
	self assert: token value equals: 'firstToken'.

	token := scanner next.
	self assert: token isBinary.
	self assert: token value equals: '+'
]

{ #category : 'tests - Identifiers' }
ASTScannerTest >> testNextIdentifierCanNotHaveBinary [
	| scanner token |
	scanner := self buildScannerForText: 'first+Token'.

	token := scanner next.
	self assert: token isIdentifier.
	self assert: token value equals: 'first'.

	token := scanner next.
	self assert: token isBinary.
	self assert: token value equals: '+'.

	token := scanner next.
	self assert: token isIdentifier.
	self assert: token value equals: 'Token'
]

{ #category : 'tests - Start' }
ASTScannerTest >> testNextIdentifierStartAtFirstLetter [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken'.
	token := scanner next.
	self assert: token start equals: 1
]

{ #category : 'tests - Stop' }
ASTScannerTest >> testNextIdentifierStopAtLastLetter [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken'.
	token := scanner next.
	self assert: token stop equals: 10
]

{ #category : 'tests - Identifiers' }
ASTScannerTest >> testNextIdentifierValueIsName [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken'.
	token := scanner next.
	self assert: token value equals: 'firstToken'
]

{ #category : 'tests - Start' }
ASTScannerTest >> testNextKeyWordStartAtFirstLetter [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken:'.
	token := scanner next.
	self assert: token start equals: 1
]

{ #category : 'tests - Keyword' }
ASTScannerTest >> testNextKeywordBeginningWithSpecialCharacterIsNotKeyword [
	| scanner token |
	scanner := self buildScannerForText: '^firstToken:'.
	token := scanner next.
	self assert: token isSpecial.
	self assert: token value equals: $^.

	scanner := self buildScannerForText: '$firstToken:'.
	token := scanner next.
	self assert: token isLiteralToken.
	self assert: token value equals: $f.

	scanner := self buildScannerForText: '\firstToken:'.
	token := scanner next.
	self assert: token isBinary.
	self assert: token value equals: '\'.

	scanner := self buildScannerForText: '`firstToken:'.
	token := scanner next.
	self assert: token isError.
	self assert: token value equals: '`'
]

{ #category : 'tests - Keyword' }
ASTScannerTest >> testNextKeywordCanContainUnderscore [
	| scanner token |
	scanner := self buildScannerForText: 'first_Token:'.
	token := scanner next.
	self assert: token isKeyword.
	self assert: token value equals: 'first_Token:'.

	scanner := self buildScannerForText: '_firstToken:'.
	token := scanner next.
	self assert: token isKeyword.
	self assert: token value equals: '_firstToken:'.

	scanner := self buildScannerForText: '_firstToken:'.
	token := scanner next.
	self assert: token isKeyword.
	self assert: token value equals: '_firstToken:'
]

{ #category : 'tests - Keyword' }
ASTScannerTest >> testNextKeywordCanNotBeFullBinary [
	| scanner token |
	scanner := self buildScannerForText: '+-=:'.

	"The scanner doesn't understand it as a keyword."
	token := scanner next.
	self assert: token isBinary.
	self assert: token value equals: '+-='.

	"The colon is therefor identified as a special character as it stands alone."
	token := scanner next.
	self assert: token isSpecial.
	self assert: token value equals: $:
]

{ #category : 'tests - Keyword' }
ASTScannerTest >> testNextKeywordCanNotBeFullDigit [
	| scanner token |
	scanner := self buildScannerForText: '123:'.

	"The scanner doesn't understand it as a keyword."
	token := scanner next.
	self assert: token isNumberLiteralToken.
	self assert: token value equals: 123.

	"The colon is therefor identified as a special character as it stands alone."
	token := scanner next.
	self assert: token isSpecial.
	self assert: token value equals: $:
]

{ #category : 'tests - Keyword' }
ASTScannerTest >> testNextKeywordCanNotBeginWithBinary [
	| scanner token |
	scanner := self buildScannerForText: '+alpha:'.

	token := scanner next.
	self assert: token isBinary.
	self assert: token value equals: '+'.

	token := scanner next.
	self assert: token isKeyword.
	self assert: token value equals: 'alpha:'
]

{ #category : 'tests - Keyword' }
ASTScannerTest >> testNextKeywordCanNotBeginWithDigit [
	| scanner token |
	scanner := self buildScannerForText: '123alpha:'.

	token := scanner next.
	self assert: token isNumberLiteralToken.
	self assert: token value equals: 123.

	token := scanner next.
	self assert: token isKeyword.
	self assert: token value equals: 'alpha:'
]

{ #category : 'tests - Stop' }
ASTScannerTest >> testNextKeywordStopAtColon [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken:'.
	token := scanner next.
	self assert: token isKeyword.
	self assert: token stop equals: 11.

	scanner := self buildScannerForText: 'first:Token'.
	token := scanner next.
	self assert: token isKeyword.
	self assert: token stop equals: 6
]

{ #category : 'tests - Keyword' }
ASTScannerTest >> testNextKeywordValueIsStartToStop [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken:'.
	token := scanner next.
	self assert: token value equals: 'firstToken:'.

	scanner := self buildScannerForText: 'first:Token'.
	token := scanner next.
	self assert: token value equals: 'first:'
]

{ #category : 'tests - SpecialCharacters' }
ASTScannerTest >> testNextLiteralArrayHasRightValue [
	"Note: RBScanner only spots the beginning of the array."
	| scanner token |

	scanner := self buildScannerForText: '#['.
	token := scanner next.
	self assert: token value equals: '#['
]

{ #category : 'tests - Start' }
ASTScannerTest >> testNextLiteralArrayWithBracketStartAtHashtag [
	"Note: RBScanner only spots the beginning of the array."
	| scanner token |

	scanner := self buildScannerForText: '#['.
	token := scanner next.
	self assert: token start equals: 1
]

{ #category : 'tests - Stop' }
ASTScannerTest >> testNextLiteralArrayWithBracketStopAtBracket [
	"Note: RBScanner only spots the beginning of the array."
	| scanner token |

	scanner := self buildScannerForText: '#['.
	token := scanner next.
	self assert: token stop equals: 2
]

{ #category : 'tests - SpecialCharacters' }
ASTScannerTest >> testNextLiteralArrayWithParenthesisHasRigthValue [
	"Note: RBScanner only spots the beginning of the array."
	| scanner token |
	scanner := self buildScannerForText: '#('.
	token := scanner next.
	self assert: token value equals: '#('
]

{ #category : 'tests - Start' }
ASTScannerTest >> testNextLiteralArrayWithParenthesisStartAtHashTag [
	"Note: RBScanner only spots the beginning of the array."
	| scanner token |
	scanner := self buildScannerForText: '#('.
	token := scanner next.
	self assert: token start equals: 1
]

{ #category : 'tests - Stop' }
ASTScannerTest >> testNextLiteralArrayWithParenthesisStopAtParenthesis [
	"Note: RBScanner only spots the beginning of the array."
	| scanner token |
	scanner := self buildScannerForText: '#('.
	token := scanner next.
	self assert: token stop equals: 2
]

{ #category : 'tests - Error' }
ASTScannerTest >> testNextLiteralBeginningWithApostropheThrowsError [
	| scanner token |
	scanner := self buildScannerForText: '#'''.
	token := scanner next.
	self assert: token isError.
	self assert: token value equals: #'#'''.
	self assert: token cause equals: 'Unmatched '' in string literal.'
]

{ #category : 'tests - Error' }
ASTScannerTest >> testNextLiteralBeginningWithDigitThrowsError [
	| scanner token |
	scanner := self buildScannerForText: '#123alpha'.
	token := scanner next.
	self assert: token isError.
	self assert: token value equals: '#'.
	self assert: token cause equals: 'Literal expected'
]

{ #category : 'tests - Error' }
ASTScannerTest >> testNextLiteralBeginningWithSpecialThrowsError [
	| scanner token |
	scanner := self buildScannerForText: '#^'.
	token := scanner next.
	self assert: token isError.
	self assert: token value equals: '#'.
	self assert: token cause equals: 'Literal expected'
]

{ #category : 'tests - Error' }
ASTScannerTest >> testNextLiteralBeginningWithUnknownThrowsError [
	| scanner token |
	scanner := self buildScannerForText: '#`'.
	token := scanner next.
	self assert: token isError.
	self assert: token value equals: '#'.
	self assert: token cause equals: 'Literal expected'
]

{ #category : 'tests - Literal' }
ASTScannerTest >> testNextLiteralCanBeBinary [
	| scanner token |
	scanner := self buildScannerForText: '#+=-/'.
	token := scanner next.
	self assert: token isLiteralToken.
	self assert: token value equals: #+=-/
]

{ #category : 'tests - Literal' }
ASTScannerTest >> testNextLiteralCanContainColon [
	| scanner token |

	scanner := self buildScannerForText: '#al:pha'.
	token := scanner next.
	self assert: token isLiteralToken.
	self assert: token value equals: #'al:pha'
]

{ #category : 'tests - Literal' }
ASTScannerTest >> testNextLiteralCanContainDigits [
	| scanner token |
	scanner := self buildScannerForText: '#alpha123beta456'.
	token := scanner next.
	self assert: token isLiteralToken.
	self assert: token value equals: #alpha123beta456
]

{ #category : 'tests - Literal' }
ASTScannerTest >> testNextLiteralCanHaveMultipleHashtag [
	| scanner token |
	scanner := self buildScannerForText: '########literal'.
	token := scanner next.
	self assert: token isLiteralToken
]

{ #category : 'tests - Literal' }
ASTScannerTest >> testNextLiteralCanNotContainBinaryAndAlphabetic [
	| scanner token |
	scanner := self buildScannerForText: '#+alpha'.
	token := scanner next.
	self assert: token value equals: #+.

	scanner := self buildScannerForText: '#al=pha'.
	token := scanner next.
	self assert: token value equals: #al.

	scanner := self buildScannerForText: '#alpha-'.
	token := scanner next.
	self assert: token value equals: #alpha
]

{ #category : 'tests - Literal' }
ASTScannerTest >> testNextLiteralCharacterCanBeAnyCharacter [
	| scanner token |
	scanner := self buildScannerForText: '$1'.
	token := scanner next.
	self assert: token isLiteralToken.

	scanner := self buildScannerForText: '$a'.
	token := scanner next.
	self assert: token isLiteralToken.

	scanner := self buildScannerForText: '$+'.
	token := scanner next.
	self assert: token isLiteralToken.

	scanner := self buildScannerForText: '$#'.
	token := scanner next.
	self assert: token isLiteralToken.

	scanner := self buildScannerForText: '$$'.
	token := scanner next.
	self assert: token isLiteralToken.

	scanner := self buildScannerForText: '$^'.
	token := scanner next.
	self assert: token isLiteralToken.

	scanner := self buildScannerForText: '$`'.
	token := scanner next.
	self assert: token isLiteralToken
]

{ #category : 'tests - Literal' }
ASTScannerTest >> testNextLiteralCharacterHasCharacterAsValue [
	| scanner token |
	scanner := self buildScannerForText: '$1'.
	token := scanner next.
	self assert: token value equals: $1.

	scanner := self buildScannerForText: '$a'.
	token := scanner next.
	self assert: token value equals: $a.

	scanner := self buildScannerForText: '$+'.
	token := scanner next.
	self assert: token value equals: $+.

	scanner := self buildScannerForText: '$#'.
	token := scanner next.
	self assert: token value equals: $#.

	scanner := self buildScannerForText: '$$'.
	token := scanner next.
	self assert: token value equals: $$.

	scanner := self buildScannerForText: '$`'.
	token := scanner next.
	self assert: token value equals: $`.

	scanner := self buildScannerForText: '$^'.
	token := scanner next.
	self assert: token value equals: $^
]

{ #category : 'tests - Literal' }
ASTScannerTest >> testNextLiteralCharacterHasOnlyOneCharacter [
	| scanner token |
	scanner := self buildScannerForText: '$123'.
	token := scanner next.
	self assert: token value equals: $1
]

{ #category : 'tests' }
ASTScannerTest >> testNextLiteralCharacterWithAMissingCharacter [
	| source scanner scannedToken |
	source := '$'.
	scanner := self buildScannerForText: source.
	scannedToken := scanner next.
	self verifyErrorToken: scannedToken message: 'Character expected' translated valueExpected: '$'
]

{ #category : 'tests - Start' }
ASTScannerTest >> testNextLiteralStartAtFirstLetter [
	| scanner token |
	scanner := self buildScannerForText: '########literal'.
	token := scanner next.
	self assert: token isLiteralToken.
	self assert: token start equals: 1
]

{ #category : 'tests - Stop' }
ASTScannerTest >> testNextLiteralStopAtLastLetter [
	| scanner token |
	scanner := self buildScannerForText: '########literal'.
	token := scanner next.
	self assert: token isLiteralToken.
	self assert: token stop equals: 15
]

{ #category : 'tests - Literal' }
ASTScannerTest >> testNextLiteralWithMultipleHashtagTrimThem [
	| scanner token |
	scanner := self buildScannerForText: '########literal'.
	token := scanner next.
	self assert: token value equals: 'literal'
]

{ #category : 'tests - Keyword' }
ASTScannerTest >> testNextMultipleKeywordColonIsSufficientAfterFirstKeyword [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken::'.
	token := scanner next.
	self assert: token isKeyword.
	self assert: token value equals: 'firstToken:'.

	scanner := self buildScannerForText: 'firstToken::::::'.
	token := scanner next.
	self assert: token isKeyword.
	self assert: token value equals: 'firstToken:'
]

{ #category : 'tests - Start' }
ASTScannerTest >> testNextMultipleKeywordStartAtFirstLetter [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken:secondToken:'.
	token := scanner next.
	self assert: token isKeyword.
	self assert: token start equals: 1
]

{ #category : 'tests - Stop' }
ASTScannerTest >> testNextMultipleKeywordStopAtLastLetter [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken:secondToken:'.
	token := scanner next.
	self assert: token isKeyword.
	self assert: token stop equals: 11
]

{ #category : 'tests - NumberLiteral' }
ASTScannerTest >> testNextNegativeFloatNumbersIsNumberLiteral [

	| scanner token |
	scanner := self buildScannerForText: '-3.2'.
	token := scanner next.
	self assert: token isNumberLiteralToken
]

{ #category : 'tests - NumberLiteral' }
ASTScannerTest >> testNextNegativeNumberCanNotHaveSpaces [

	| scanner token |
	scanner := self buildScannerForText: '- 12'.

	token := scanner next.
	self assert: token isBinary.
	self assert: token value equals: '-'.

	token := scanner next.
	self assert: token isNumberLiteralToken.
	self assert: token value equals: 12
]

{ #category : 'tests - NumberLiteral' }
ASTScannerTest >> testNextNegativeNumberIsNumberLiteral [

	| scanner token |
	scanner := self buildScannerForText: '-12'.
	token := scanner next.
	self assert: token isNumberLiteralToken
]

{ #category : 'tests - Literal' }
ASTScannerTest >> testNextNilIsLiteral [
	| scanner token |
	scanner := self buildScannerForText: 'nil'.
	token := scanner next.
	self assert: token isLiteralToken.
	self assert: token value equals: nil
]

{ #category : 'tests - Start' }
ASTScannerTest >> testNextNumberLiteralStartAtTheBeginning [

	| scanner token |
	scanner := self buildScannerForText: '-10r10'.
	token := scanner next.
	self assert: token start equals: 1.

	scanner := self buildScannerForText: '-2r1010'.
	token := scanner next.
	self assert: token start equals: 1.

	scanner := self buildScannerForText: '-16ra'.
	token := scanner next.
	self assert: token start equals: 1.

	scanner := self buildScannerForText: '-8r12'.
	token := scanner next.
	self assert: token start equals: 1
]

{ #category : 'tests - Stop' }
ASTScannerTest >> testNextNumberLiteralStopAtTheLastDigit [

	| scanner token expr |
	expr := '-10r10'.
	scanner := self buildScannerForText: expr.
	token := scanner next.
	self assert: token stop equals: expr size.

	expr := '-2r1010'.
	scanner := self buildScannerForText: expr.
	token := scanner next.
	self assert: token stop equals: expr size.

	expr := '-16ra'.
	scanner := self buildScannerForText: expr.
	token := scanner next.
	self assert: token stop equals: expr size.

	expr := '-8r12'.
	scanner := self buildScannerForText: expr.
	token := scanner next.
	self assert: token stop equals: expr size
]

{ #category : 'tests - NumberLiteral' }
ASTScannerTest >> testNextRadixNumbersCanBeFloatAsWell [

	| scanner token |
	scanner := self buildScannerForText: '10r10.2'.
	token := scanner next.
	self assert: token value equals: 10.2.

	scanner := self buildScannerForText: '2r1010.1'.
	token := scanner next.
	self assert: token value equals: 10.5.

	scanner := self buildScannerForText: '16ra.a'.
	token := scanner next.
	self assert: token value equals: 10.625.

	scanner := self buildScannerForText: '8r12.44'.
	token := scanner next.
	self assert: token value equals: 10.5625
]

{ #category : 'tests - NumberLiteral' }
ASTScannerTest >> testNextRadixNumbersCanBeNegative [

	| scanner token |
	scanner := self buildScannerForText: '-10r10'.
	token := scanner next.
	self assert: token value equals: -10.

	scanner := self buildScannerForText: '-2r1010'.
	token := scanner next.
	self assert: token value equals: -10.

	scanner := self buildScannerForText: '-16ra'.
	token := scanner next.
	self assert: token value equals: -10.

	scanner := self buildScannerForText: '-8r12'.
	token := scanner next.
	self assert: token value equals: -10
]

{ #category : 'tests - Separators' }
ASTScannerTest >> testNextSecondTokenDoesNotConsumePrecedingComment [
	| scanner token |
	scanner := self buildScannerForText:
	'firstToken
   "inbetween comment" secondToken
	"second inbetween" "multiple comments"'.
	"Expecting all spaces and line breaks to be trimmed."

	"The first token 'firstToken' takes the previous comment and the following one"
	token := scanner next.
	"The next token 'secondToken' takes the following 2 comments."
	token := scanner next.
	self assert: token comments size equals: 2.
	self assert: token start equals: 35
]

{ #category : 'tests - Start' }
ASTScannerTest >> testNextSpecialCharacterStartAtTheBeginning [
	| scanner token |
	scanner := self buildScannerForText: '(]'.
	token := scanner next.
	self assert: token start equals: 1.

	scanner := self buildScannerForText: '^('.
	token := scanner next.
	token := scanner next.
	self assert: token start equals: 2.

	scanner := self buildScannerForText: ']'.
	token := scanner next.
	self assert: token start equals: 1
]

{ #category : 'tests - Stop' }
ASTScannerTest >> testNextSpecialCharacterStopAtThemself [
	| scanner token |
	scanner := self buildScannerForText: '(]'.
	token := scanner next.
	self assert: token stop equals: 1.

	scanner := self buildScannerForText: '^('.
	token := scanner next.
	token := scanner next.
	self assert: token stop equals: 2.

	scanner := self buildScannerForText: ']'.
	token := scanner next.
	self assert: token stop equals: 1
]

{ #category : 'tests - SpecialCharacters' }
ASTScannerTest >> testNextSpecialCharactereCanBeAssignement [
	| scanner token |
	scanner := self buildScannerForText: ':='.
	token := scanner next.
	self assert: token start equals: 1
]

{ #category : 'tests - SpecialCharacters' }
ASTScannerTest >> testNextSpecialCharactereisSingle [
	| scanner token |
	scanner := self buildScannerForText: '(]'.
	token := scanner next.
	self assert: token value equals: $(.

	scanner := self buildScannerForText: '^('.
	token := scanner next.
	self assert: token value equals: $^.

	scanner := self buildScannerForText: ']'.
	token := scanner next.
	self assert: token value equals: $]
]

{ #category : 'tests - SpecialCharacters' }
ASTScannerTest >> testNextSpecialCharactereisSpecial [
	| scanner token |
	scanner := self buildScannerForText: '('.
	token := scanner next.
	self assert: token isSpecial.

	scanner := self buildScannerForText: '^'.
	token := scanner next.
	self assert: token isSpecial.

	scanner := self buildScannerForText: ']'.
	token := scanner next.
	self assert: token isSpecial
]

{ #category : 'tests - Literal' }
ASTScannerTest >> testNextStringCanContainOtherString [
	| scanner token |
	scanner := self buildScannerForText: ''' The world has changed ''''I can feel it in the water'''' '''.
	token := scanner next.
	self assert: token isLiteralToken.
	self assert: token value equals: ' The world has changed ''I can feel it in the water'' '
]

{ #category : 'tests - Literal' }
ASTScannerTest >> testNextStringContainsAllTheContent [
	| scanner token |
	scanner := self buildScannerForText: '''The world has changed'''.
	token := scanner next.
	self assert: token value equals: 'The world has changed'
]

{ #category : 'tests - Literal' }
ASTScannerTest >> testNextStringErrorDoesNotPropagateInside [
	| scanner token |
	scanner := self buildScannerForText: ''' The world has changed ''''I feel it in the water'''.
	token := scanner next.
	self assert: token value equals: ' The world has changed ''I feel it in the water'
]

{ #category : 'tests - Literal' }
ASTScannerTest >> testNextStringIsLiteral [
	| scanner token |
	scanner := self buildScannerForText: '''the world has changed'''.
	token := scanner next.
	self assert: token isLiteralToken
]

{ #category : 'tests - Start' }
ASTScannerTest >> testNextTokenStartAfterComment [
	"Tests that the next token adapts starting point according to it."
	| scanner token |
	scanner := self buildScannerForText: '"start comment"firstToken'.
	token := scanner next.
	self assert: token value equals: 'firstToken'.
	self assert: token start equals: 16
]

{ #category : 'tests - Start' }
ASTScannerTest >> testNextTokenStartAfterLineBreak [
	"Tests that the next token adapts starting point according to it."
	| scanner token |
	scanner := self buildScannerForText: '
firstToken'.
	token := scanner next.
	self assert: token value equals: 'firstToken'.
	self assert: token start equals: 2
]

{ #category : 'tests - Start' }
ASTScannerTest >> testNextTokenStartAfterSpaces [
	"Tests that the next token adapts starting point according to it."
	| scanner token |
	scanner := self buildScannerForText: String space, String space, 'firstToken'.
	token := scanner next.
	self assert: token value equals: 'firstToken'.
	self assert: token start equals: 3
]

{ #category : 'tests - Start' }
ASTScannerTest >> testNextTokenStartAfterTab [
	"Tests that the next token adapts starting point according to it."
	| scanner token |
	scanner := self buildScannerForText: String tab, 'firstToken'.
	token := scanner next.
	self assert: token value equals: 'firstToken'.
	self assert: token start equals: 2
]

{ #category : 'tests - Separators' }
ASTScannerTest >> testNextTrimsBeginningComment [
	| scanner token |
	scanner := self buildScannerForText: '"start comment"firstToken'.
	token := scanner next.
	self assert: token value equals: 'firstToken'
]

{ #category : 'tests - Separators' }
ASTScannerTest >> testNextTrimsBeginningLineBreak [
	| scanner token |
	scanner := self buildScannerForText: '
firstToken'.
	token := scanner next.
	self assert: token value equals: 'firstToken'
]

{ #category : 'tests - Separators' }
ASTScannerTest >> testNextTrimsBeginningSpaces [
	| scanner token |
	scanner := self buildScannerForText: String space, String space, 'firstToken'.
	token := scanner next.
	self assert: token value equals: 'firstToken'
]

{ #category : 'tests - Separators' }
ASTScannerTest >> testNextTrimsBeginningTab [
	| scanner token |
	scanner := self buildScannerForText: String tab, 'firstToken'.
	token := scanner next.
	self assert: token value equals: 'firstToken'
]

{ #category : 'tests - Separators' }
ASTScannerTest >> testNextTrimsEndComment [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken"end comment"'.
	scanner next.
	token := scanner next.
	self assert: token isEOF.
	self assert: token start equals: 24
]

{ #category : 'tests - Separators' }
ASTScannerTest >> testNextTrimsEndLineBreak [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken
'.
	scanner next.
	token := scanner next.
	self assert: token isEOF.
	self assert: token start equals: 12
]

{ #category : 'tests - Separators' }
ASTScannerTest >> testNextTrimsEndSpaces [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken', String space, String space.
	scanner next.
	token := scanner next.
	self assert: token isEOF.
	self assert: token start equals: 13
]

{ #category : 'tests - Separators' }
ASTScannerTest >> testNextTrimsEndTab [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken', String tab.
	scanner next.
	token := scanner next.
	self assert: token isEOF.
	self assert: token start equals: 12
]

{ #category : 'tests - Separators' }
ASTScannerTest >> testNextTrimsMiddleComment [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken"middle comment"secondToken'.
	scanner next.
	token := scanner next.
	self assert: token isIdentifier.
	self assert: token start equals: 27
]

{ #category : 'tests - Separators' }
ASTScannerTest >> testNextTrimsMiddleLineBreak [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken
secondToken'.
	scanner next.
	token := scanner next.
	self assert: token isIdentifier.
	self assert: token start equals: 12
]

{ #category : 'tests - Separators' }
ASTScannerTest >> testNextTrimsMiddleSpaces [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken',String space, String space,'secondToken'.
	scanner next.
	token := scanner next.
	self assert: token isIdentifier.
	self assert: token start equals: 13
]

{ #category : 'tests - Separators' }
ASTScannerTest >> testNextTrimsMiddleTab [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken', String tab, 'secondToken'.
	scanner next.
	token := scanner next.
	self assert: token isIdentifier.
	self assert: token start equals: 12
]

{ #category : 'tests - Literal' }
ASTScannerTest >> testNextTrueIsLiteral [
	| scanner token |
	scanner := self buildScannerForText: 'true'.
	token := scanner next.
	self assert: token isLiteralToken.
	self assert: token value equals: true
]

{ #category : 'tests - Keyword' }
ASTScannerTest >> testNextTwoKeywordIsMultipleKeyword [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken:secondToken:'.
	token := scanner next.
	self assert: token isKeyword.
	self assert: token value equals: 'firstToken:'
]

{ #category : 'tests - Identifiers' }
ASTScannerTest >> testNextUnderscoreWithOnlyDigitsIsIdentifier [

	"This test shows that the underscore is an alphabetic character."
	| scanner token |
	scanner := self buildScannerForText: '_123'.
	token := scanner next.
	self assert: token isIdentifier.
	self assert: token value equals: '_123'
]

{ #category : 'tests - Error' }
ASTScannerTest >> testNextUnknownCharacterThrowsError [
	| scanner token |
	scanner := self buildScannerForText: '`'.
	token := scanner next.
	self assert: token isError.
	self assert: token value equals: '`'.
	self assert: token cause equals: 'Unknown character'
]

{ #category : 'tests - NumberLiteral' }
ASTScannerTest >> testNextValueOfNegativeNumbers [

	| scanner token |
	scanner := self buildScannerForText: '-12'.
	token := scanner next.
	self assert: token value equals: -12.

	scanner := self buildScannerForText: '-3'.
	token := scanner next.
	self assert: token value equals: -3
]

{ #category : 'tests - NumberLiteral' }
ASTScannerTest >> testNextValueOfRadixNumbersCutsWhenValueIsImpossible [

	| scanner token |
	scanner := self buildScannerForText: '10r1a'.
	token := scanner next.
	self assert: token value equals: 1.

	scanner := self buildScannerForText: '2r10210'.
	token := scanner next.
	self assert: token value equals: 2.

	scanner := self buildScannerForText: '16r3g'.
	token := scanner next.
	self assert: token value equals: 3.

	scanner := self buildScannerForText: '8r192'.
	token := scanner next.
	self assert: token value equals: 1
]

{ #category : 'tests - NumberLiteral' }
ASTScannerTest >> testNextValueOfRadixNumbersIsConvertedToDecimal [

	| scanner token |
	scanner := self buildScannerForText: '10r10'.
	token := scanner next.
	self assert: token value equals: 10.

	scanner := self buildScannerForText: '2r1010'.
	token := scanner next.
	self assert: token value equals: 10.

	scanner := self buildScannerForText: '16ra'.
	token := scanner next.
	self assert: token value equals: 10.

	scanner := self buildScannerForText: '8r12'.
	token := scanner next.
	self assert: token value equals: 10
]

{ #category : 'tests' }
ASTScannerTest >> testNextWithAWrongSymbolGetError [
	| source scanner scannedToken |
	source := '#^'.
	scanner := self buildScannerForText: source.
	scannedToken := scanner next.
	self verifyErrorToken: scannedToken message: 'Literal expected' translated valueExpected: '#'
]

{ #category : 'tests' }
ASTScannerTest >> testNextWithAnIdentifierTokenGetTheIdentifierToken [
	| source scanner token |
	source := 'identifierToken'.
	scanner := self buildScannerForText: source.
	token := scanner next.
	self assert: token isIdentifier.
	self assert: token value equals: source
]

{ #category : 'tests' }
ASTScannerTest >> testNextWithAnOpeningCommentAtEndGetError [
	| source token |
	source := 'self "'.
	token := (self buildScannerForText: source) next; next.
	self assert: token isError.
	self assert: token cause equals: 'Unmatched " in comment.'.
	self assert: token value equals: '"'
]

{ #category : 'tests' }
ASTScannerTest >> testNextWithAnOpeningCommentGetError [
	| source token |
	source := '"only the opening'.
	token := (self buildScannerForText: source) next.
	self assert: token isError.
	self assert: token cause equals: 'Unmatched " in comment.'.
	self assert: token value equals: source
]

{ #category : 'tests' }
ASTScannerTest >> testNextWithAnOpeningStringsGetError [
	| source scannedToken |
	source := '''only the opening'.
	scannedToken := (self buildScannerForText: source) next.
	self verifyErrorToken: scannedToken message: 'Unmatched '' in string literal.' translated valueExpected: source
]

{ #category : 'tests' }
ASTScannerTest >> testNextWithAnUnknownCharacterGetError [
	| source scanner scannedToken |
	source := '¿'.
	scanner := self buildScannerForText: source.
	scannedToken := scanner next.
	self verifyErrorToken: scannedToken message: 'Unknown character' translated valueExpected: source
]

{ #category : 'tests - Comment' }
ASTScannerTest >> testNextWithTwoDoubleQuotesInComment [
	| source token |
	source := '"only the"" opening"'.
	self
		shouldnt: [ token := (self buildScannerForText: source) next ]
		raise: CodeError.
	self assert: token comments first value equals: 'only the" opening'
]

{ #category : 'tests' }
ASTScannerTest >> testNextWithTwoDoubleQuotesInCommentGetError [
	| source token |
	source := '"only the"" opening'.
	token := (self buildScannerForText: source) next.
	self assert: token isError.
	self assert: token cause equals: 'Unmatched " in comment.'.
	self assert: token value equals: source
]

{ #category : 'tests - Error' }
ASTScannerTest >> testNextWithUnFinishedCommentGivesAnError [

	| source token |
	source := 'firstToken "unfinished comment secondToken'.
	token := (self buildScannerForText: source) next; next.
	self assert: token isError.
	self assert: token cause equals: 'Unmatched " in comment.'.
	self assert: token value equals: '"unfinished comment secondToken'
]

{ #category : 'tests - Creation api' }
ASTScannerTest >> testScanTokenObjects1 [
	| inp exp |
	inp := 'Object subclass: #NameOfSubclass'.
	exp := {'Object'.
	'subclass:'.
	#NameOfSubclass asString}.
	self assert: ((self scannerClass scanTokenObjects: inp) collect: [ :each | each value ]) equals: exp
]

{ #category : 'tests - Creation api' }
ASTScannerTest >> testScanTokenObjects2 [
	| inp exp |
	inp := 'classVariableNames: '''' "ha ha"
package: ''UndefinedClasses-Experiment'.
	exp := {'classVariableNames:' . '' . 'package:' . '''UndefinedClasses-Experiment'}.
	self assert: ((self scannerClass scanTokenObjects: inp) collect: [ :each | each value ]) equals: exp
]

{ #category : 'utilities' }
ASTScannerTest >> verifyErrorToken: scannedToken message: message valueExpected: valueExpected [
	self assert: scannedToken isError.
	self assert: scannedToken cause equals: message.
	self assert: scannedToken value equals: valueExpected.
	self assert: scannedToken start equals: 1
]
