"
SUnit tests for RBScanner
"
Class {
	#name : 'OCScannerTest',
	#superclass : 'TestCase',
	#category : 'AST-Core-Tests-Parser',
	#package : 'AST-Core-Tests',
	#tag : 'Parser'
}

{ #category : 'initialize' }
OCScannerTest >> buildScannerForText: source [
	^ self scannerClass on: source readStream
]

{ #category : 'initialize' }
OCScannerTest >> scannerClass [
	^ OCScanner
]

{ #category : 'tests - Comment' }
OCScannerTest >> testCommentTokenHasTheRightValue [
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
OCScannerTest >> testCommentTokenStartAtFirstQuote [
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
OCScannerTest >> testCommentTokenStopAtLastQuote [
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

{ #category : 'tests - isSelector' }
OCScannerTest >> testIsValidSelector [
	self assert: (OCScanner isSelector: 'class').
	self deny: (OCScanner isSelector: '0class')
]

{ #category : 'tests - Comment' }
OCScannerTest >> testMultipleCommentTokensAreWellSeparated [
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
OCScannerTest >> testNextAcceptAllAlphabeticAsLiteral [
	| scanner token |
	scanner := self buildScannerForText: '#alpha'.
	token := scanner next.
	self assert: token isLiteralToken
]

{ #category : 'tests - NumberLiteral' }
OCScannerTest >> testNextAcceptFloatNumbers [

	| scanner token |
	scanner := self buildScannerForText: '3.2'.
	token := scanner next.
	self assert: token isNumberLiteralToken
]

{ #category : 'tests - SpecialCharacters' }
OCScannerTest >> testNextAcceptLiteralArrayWithBracket [
	"Note: RBScanner only spots the beginning of the array."
	| scanner token |

	scanner := self buildScannerForText: '#['.
	token := scanner next.
	self assert: token isLiteralArrayToken
]

{ #category : 'tests - SpecialCharacters' }
OCScannerTest >> testNextAcceptLiteralArrayWithParenthesis [
	"Note: RBScanner only spots the beginning of the array."
	| scanner token |
	scanner := self buildScannerForText: '#('.
	token := scanner next.
	self assert: token isLiteralArrayToken
]

{ #category : 'tests - NumberLiteral' }
OCScannerTest >> testNextAcceptRadixNumbers [

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
OCScannerTest >> testNextAllAlphabeticEndingWithColonGiveKeyword [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken:'.
	token := scanner next.
	self assert: token isKeyword
]

{ #category : 'tests - Identifiers' }
OCScannerTest >> testNextAllAlphabeticsGiveIdentifier [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken'.
	token := scanner next.
	self assert: token isIdentifier
]

{ #category : 'tests - Keyword' }
OCScannerTest >> testNextAlphabeticAndDigitEndingWithColonGiveKeyword [
	| scanner token |
	scanner := self buildScannerForText: 'first123Token:'.
	token := scanner next.
	self assert: token isKeyword
]

{ #category : 'tests - Identifiers' }
OCScannerTest >> testNextAlphabeticsAndDigitGiveIdentifier [
	| scanner token |
	scanner := self buildScannerForText: 'first123Token'.
	token := scanner next.
	self assert: token isIdentifier
]

{ #category : 'tests - Start' }
OCScannerTest >> testNextAssignementStartAtColon [
	| scanner token |
	scanner := self buildScannerForText: ':='.
	token := scanner next.
	self assert: token start equals: 1
]

{ #category : 'tests - Stop' }
OCScannerTest >> testNextAssignementStopAtEqual [
	| scanner token |
	scanner := self buildScannerForText: ':='.
	token := scanner next.
	self assert: token stop equals: 2
]

{ #category : 'tests - Error' }
OCScannerTest >> testNextBaseOfRadixNumbersCanNotBeOne [

	| scanner token |
	scanner := self buildScannerForText: '1r1a'.
	token := scanner next.
	self assert: token isError.
	self assert: token start equals: 1
]

{ #category : 'tests - NumberLiteral' }
OCScannerTest >> testNextBeginningWithDigitIsNumber [

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
OCScannerTest >> testNextBinaryGivesBinarySelector [
	| scanner token |
	scanner := self buildScannerForText: '+'.
	token := scanner next.
	self assert: token isBinary
]

{ #category : 'tests - BinarySelector' }
OCScannerTest >> testNextBinarySelectorCanBeMultipleBinaries [
	| scanner token |
	scanner := self buildScannerForText: '+-=/'.
	token := scanner next.
	self assert: token isBinary.
	self assert: token value equals: '+-=/'
]

{ #category : 'tests - Start' }
OCScannerTest >> testNextBinarySelectorStartAtFirstLetter [
	| scanner token |
	scanner := self buildScannerForText: '+-=/'.
	token := scanner next.
	self assert: token value equals: '+-=/'.
	self assert: token start equals: 1
]

{ #category : 'tests - Stop' }
OCScannerTest >> testNextBinarySelectorStopAtLastLetter [
	| scanner token |
	scanner := self buildScannerForText: '+-=/'.
	token := scanner next.
	self assert: token value equals: '+-=/'.
	self assert: token stop equals: 4
]

{ #category : 'tests - BinarySelector' }
OCScannerTest >> testNextBinarySelectorStopsAtNonBinary [
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
OCScannerTest >> testNextBinarySelectorValueIsBinary [
	| scanner token |
	scanner := self buildScannerForText: '+'.
	token := scanner next.
	self assert: token value equals: '+'
]

{ #category : 'tests - Comment' }
OCScannerTest >> testNextCommentIsCommentToken [
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
OCScannerTest >> testNextDoesNotAddLineBreakToComments [
	| scanner token |
	scanner := self buildScannerForText: '
firstToken'.
	token := scanner next.
	self assert: token comments isNil
]

{ #category : 'tests - Separators' }
OCScannerTest >> testNextDoesNotAddSpacesToComments [
	| scanner token |
	scanner := self buildScannerForText: String space, String space,'firstToken'.
	token := scanner next.
	self assert: token comments isNil
]

{ #category : 'tests - Separators' }
OCScannerTest >> testNextDoesNotAddTabToComments [
	| scanner token |
	scanner := self buildScannerForText: String tab, 'firstToken'.
	token := scanner next.
	self assert: token comments isNil
]

{ #category : 'tests - Error' }
OCScannerTest >> testNextDollarSignErrorHasDollarValue [
	| scanner token |
	scanner := self buildScannerForText: '$'.
	token := scanner next.
	self assert: token value equals: '$'
]

{ #category : 'tests - Error' }
OCScannerTest >> testNextDollarSignExpectedACharacter [
	| scanner token |
	scanner := self buildScannerForText: '$'.
	token := scanner next.
	self assert: token cause equals: 'Character expected'
]

{ #category : 'tests - Error' }
OCScannerTest >> testNextDollarSignIsError [
	| scanner token |
	scanner := self buildScannerForText: '$'.
	token := scanner next.
	self assert: token isError
]

{ #category : 'tests - Separators' }
OCScannerTest >> testNextEOFContainsCommentsWhenNoOtherToken [
	| scanner token |
	scanner := self buildScannerForText: '"start comment"'.

	token := scanner next.
	self assert: token isEOF.
	self assert: token comments size equals: 1.
	self assert: token start equals: 16
]

{ #category : 'tests - Start' }
OCScannerTest >> testNextErrorStartAtFirstCharacter [
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
OCScannerTest >> testNextErrorStopAtErrorCharacter [
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
OCScannerTest >> testNextFalseIsLiteral [
	| scanner token |
	scanner := self buildScannerForText: 'false'.
	token := scanner next.
	self assert: token isLiteralToken.
	self assert: token value equals: false
]

{ #category : 'tests - Separators' }
OCScannerTest >> testNextFirstTokenContainsFollowingComments [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken "following comment"'.

	"The first token 'firstToken' takes the previous comment and the following one"
	token := scanner next.
	self assert: token comments size equals: 1.
	self assert: token comments first isComment
]

{ #category : 'tests - Separators' }
OCScannerTest >> testNextFirstTokenContainsPrecedingComments [
	| scanner token |
	scanner := self buildScannerForText: '"start comment"firstToken'.
	token := scanner next.
	self assert: token comments size equals: 1.
	self assert: token comments first isComment
]

{ #category : 'tests - NumberLiteral' }
OCScannerTest >> testNextFloatNumbersCanBeNegative [

	| scanner token |
	scanner := self buildScannerForText: '-3.2'.
	token := scanner next.
	self assert: token value equals: -3.2
]

{ #category : 'tests - NumberLiteral' }
OCScannerTest >> testNextFloatNumbersCanNotHaveSpaces [

	| scanner token |
	scanner := self buildScannerForText: '3 .2'.
	token := scanner next.
	self assert: token value equals: 3.

	scanner := self buildScannerForText: '3. 2'.
	token := scanner next.
	self assert: token value equals: 3
]

{ #category : 'tests - NumberLiteral' }
OCScannerTest >> testNextFloatNumbersHasFloatValue [

	| scanner token |
	scanner := self buildScannerForText: '3.2'.
	token := scanner next.
	self assert: token value equals: 3.2
]

{ #category : 'tests - Error' }
OCScannerTest >> testNextHashtagCanNotStandAlone [
	| scanner token |
	scanner := self buildScannerForText: '#'.
	token := scanner next.
	self assert: token isError
]

{ #category : 'tests - Literal' }
OCScannerTest >> testNextHashtagStringIsLiteral [
	| scanner token |
	scanner := self buildScannerForText: '#''the world has changed'''.
	token := scanner next.
	self assert: token isLiteralToken.
	self assert: token value equals: #'the world has changed'
]

{ #category : 'tests - Identifiers' }
OCScannerTest >> testNextIdentifierCanContainUnderscore [
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
OCScannerTest >> testNextIdentifierCanNotBeginWithBinary [
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
OCScannerTest >> testNextIdentifierCanNotBeginWithDigit [
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
OCScannerTest >> testNextIdentifierCanNotEndWithBinary [
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
OCScannerTest >> testNextIdentifierCanNotHaveBinary [
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
OCScannerTest >> testNextIdentifierStartAtFirstLetter [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken'.
	token := scanner next.
	self assert: token start equals: 1
]

{ #category : 'tests - Stop' }
OCScannerTest >> testNextIdentifierStopAtLastLetter [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken'.
	token := scanner next.
	self assert: token stop equals: 10
]

{ #category : 'tests - Identifiers' }
OCScannerTest >> testNextIdentifierValueIsName [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken'.
	token := scanner next.
	self assert: token value equals: 'firstToken'
]

{ #category : 'tests - Start' }
OCScannerTest >> testNextKeyWordStartAtFirstLetter [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken:'.
	token := scanner next.
	self assert: token start equals: 1
]

{ #category : 'tests - Keyword' }
OCScannerTest >> testNextKeywordBeginningWithSpecialCharacterIsNotKeyword [
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
OCScannerTest >> testNextKeywordCanContainUnderscore [
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
OCScannerTest >> testNextKeywordCanNotBeFullBinary [
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
OCScannerTest >> testNextKeywordCanNotBeFullDigit [
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
OCScannerTest >> testNextKeywordCanNotBeginWithBinary [
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
OCScannerTest >> testNextKeywordCanNotBeginWithDigit [
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
OCScannerTest >> testNextKeywordStopAtColon [
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
OCScannerTest >> testNextKeywordValueIsStartToStop [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken:'.
	token := scanner next.
	self assert: token value equals: 'firstToken:'.

	scanner := self buildScannerForText: 'first:Token'.
	token := scanner next.
	self assert: token value equals: 'first:'
]

{ #category : 'tests - SpecialCharacters' }
OCScannerTest >> testNextLiteralArrayHasRightValue [
	"Note: RBScanner only spots the beginning of the array."
	| scanner token |

	scanner := self buildScannerForText: '#['.
	token := scanner next.
	self assert: token value equals: '#['
]

{ #category : 'tests - Start' }
OCScannerTest >> testNextLiteralArrayWithBracketStartAtHashtag [
	"Note: RBScanner only spots the beginning of the array."
	| scanner token |

	scanner := self buildScannerForText: '#['.
	token := scanner next.
	self assert: token start equals: 1
]

{ #category : 'tests - Stop' }
OCScannerTest >> testNextLiteralArrayWithBracketStopAtBracket [
	"Note: RBScanner only spots the beginning of the array."
	| scanner token |

	scanner := self buildScannerForText: '#['.
	token := scanner next.
	self assert: token stop equals: 2
]

{ #category : 'tests - SpecialCharacters' }
OCScannerTest >> testNextLiteralArrayWithParenthesisHasRigthValue [
	"Note: RBScanner only spots the beginning of the array."
	| scanner token |
	scanner := self buildScannerForText: '#('.
	token := scanner next.
	self assert: token value equals: '#('
]

{ #category : 'tests - Start' }
OCScannerTest >> testNextLiteralArrayWithParenthesisStartAtHashTag [
	"Note: RBScanner only spots the beginning of the array."
	| scanner token |
	scanner := self buildScannerForText: '#('.
	token := scanner next.
	self assert: token start equals: 1
]

{ #category : 'tests - Stop' }
OCScannerTest >> testNextLiteralArrayWithParenthesisStopAtParenthesis [
	"Note: RBScanner only spots the beginning of the array."
	| scanner token |
	scanner := self buildScannerForText: '#('.
	token := scanner next.
	self assert: token stop equals: 2
]

{ #category : 'tests - Error' }
OCScannerTest >> testNextLiteralBeginningWithApostropheThrowsError [
	| scanner token |
	scanner := self buildScannerForText: '#'''.
	token := scanner next.
	self assert: token isError.
	self assert: token value equals: #'#'''.
	self assert: token cause equals: 'Unmatched '' in string literal.'
]

{ #category : 'tests - Error' }
OCScannerTest >> testNextLiteralBeginningWithDigitThrowsError [
	| scanner token |
	scanner := self buildScannerForText: '#123alpha'.
	token := scanner next.
	self assert: token isError.
	self assert: token value equals: '#'.
	self assert: token cause equals: 'Literal expected'
]

{ #category : 'tests - Error' }
OCScannerTest >> testNextLiteralBeginningWithSpecialThrowsError [
	| scanner token |
	scanner := self buildScannerForText: '#^'.
	token := scanner next.
	self assert: token isError.
	self assert: token value equals: '#'.
	self assert: token cause equals: 'Literal expected'
]

{ #category : 'tests - Error' }
OCScannerTest >> testNextLiteralBeginningWithUnknownThrowsError [
	| scanner token |
	scanner := self buildScannerForText: '#`'.
	token := scanner next.
	self assert: token isError.
	self assert: token value equals: '#'.
	self assert: token cause equals: 'Literal expected'
]

{ #category : 'tests - Literal' }
OCScannerTest >> testNextLiteralCanBeBinary [
	| scanner token |
	scanner := self buildScannerForText: '#+=-/'.
	token := scanner next.
	self assert: token isLiteralToken.
	self assert: token value equals: #+=-/
]

{ #category : 'tests - Literal' }
OCScannerTest >> testNextLiteralCanContainColon [
	| scanner token |

	scanner := self buildScannerForText: '#al:pha'.
	token := scanner next.
	self assert: token isLiteralToken.
	self assert: token value equals: #'al:pha'
]

{ #category : 'tests - Literal' }
OCScannerTest >> testNextLiteralCanContainDigits [
	| scanner token |
	scanner := self buildScannerForText: '#alpha123beta456'.
	token := scanner next.
	self assert: token isLiteralToken.
	self assert: token value equals: #alpha123beta456
]

{ #category : 'tests - Literal' }
OCScannerTest >> testNextLiteralCanHaveMultipleHashtag [
	| scanner token |
	scanner := self buildScannerForText: '########literal'.
	token := scanner next.
	self assert: token isLiteralToken
]

{ #category : 'tests - Literal' }
OCScannerTest >> testNextLiteralCanNotContainBinaryAndAlphabetic [
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
OCScannerTest >> testNextLiteralCharacterCanBeAnyCharacter [
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
OCScannerTest >> testNextLiteralCharacterHasCharacterAsValue [
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
OCScannerTest >> testNextLiteralCharacterHasOnlyOneCharacter [
	| scanner token |
	scanner := self buildScannerForText: '$123'.
	token := scanner next.
	self assert: token value equals: $1
]

{ #category : 'tests' }
OCScannerTest >> testNextLiteralCharacterWithAMissingCharacter [
	| source scanner scannedToken |
	source := '$'.
	scanner := self buildScannerForText: source.
	scannedToken := scanner next.
	self verifyErrorToken: scannedToken message: 'Character expected' translated valueExpected: '$'
]

{ #category : 'tests - Start' }
OCScannerTest >> testNextLiteralStartAtFirstLetter [
	| scanner token |
	scanner := self buildScannerForText: '########literal'.
	token := scanner next.
	self assert: token isLiteralToken.
	self assert: token start equals: 1
]

{ #category : 'tests - Stop' }
OCScannerTest >> testNextLiteralStopAtLastLetter [
	| scanner token |
	scanner := self buildScannerForText: '########literal'.
	token := scanner next.
	self assert: token isLiteralToken.
	self assert: token stop equals: 15
]

{ #category : 'tests - Literal' }
OCScannerTest >> testNextLiteralWithMultipleHashtagTrimThem [
	| scanner token |
	scanner := self buildScannerForText: '########literal'.
	token := scanner next.
	self assert: token value equals: 'literal'
]

{ #category : 'tests - Keyword' }
OCScannerTest >> testNextMultipleKeywordColonIsSufficientAfterFirstKeyword [
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
OCScannerTest >> testNextMultipleKeywordStartAtFirstLetter [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken:secondToken:'.
	token := scanner next.
	self assert: token isKeyword.
	self assert: token start equals: 1
]

{ #category : 'tests - Stop' }
OCScannerTest >> testNextMultipleKeywordStopAtLastLetter [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken:secondToken:'.
	token := scanner next.
	self assert: token isKeyword.
	self assert: token stop equals: 11
]

{ #category : 'tests - NumberLiteral' }
OCScannerTest >> testNextNegativeFloatNumbersIsNumberLiteral [

	| scanner token |
	scanner := self buildScannerForText: '-3.2'.
	token := scanner next.
	self assert: token isNumberLiteralToken
]

{ #category : 'tests - NumberLiteral' }
OCScannerTest >> testNextNegativeNumberCanNotHaveSpaces [

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
OCScannerTest >> testNextNegativeNumberIsNumberLiteral [

	| scanner token |
	scanner := self buildScannerForText: '-12'.
	token := scanner next.
	self assert: token isNumberLiteralToken
]

{ #category : 'tests - Literal' }
OCScannerTest >> testNextNilIsLiteral [
	| scanner token |
	scanner := self buildScannerForText: 'nil'.
	token := scanner next.
	self assert: token isLiteralToken.
	self assert: token value equals: nil
]

{ #category : 'tests - Start' }
OCScannerTest >> testNextNumberLiteralStartAtTheBeginning [

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
OCScannerTest >> testNextNumberLiteralStopAtTheLastDigit [

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
OCScannerTest >> testNextRadixNumbersCanBeFloatAsWell [

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
OCScannerTest >> testNextRadixNumbersCanBeNegative [

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
OCScannerTest >> testNextSecondTokenDoesNotConsumePrecedingComment [
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
OCScannerTest >> testNextSpecialCharacterStartAtTheBeginning [
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
OCScannerTest >> testNextSpecialCharacterStopAtThemself [
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
OCScannerTest >> testNextSpecialCharactereCanBeAssignement [
	| scanner token |
	scanner := self buildScannerForText: ':='.
	token := scanner next.
	self assert: token start equals: 1
]

{ #category : 'tests - SpecialCharacters' }
OCScannerTest >> testNextSpecialCharactereisSingle [
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
OCScannerTest >> testNextSpecialCharactereisSpecial [
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
OCScannerTest >> testNextStringCanContainOtherString [
	| scanner token |
	scanner := self buildScannerForText: ''' The world has changed ''''I can feel it in the water'''' '''.
	token := scanner next.
	self assert: token isLiteralToken.
	self assert: token value equals: ' The world has changed ''I can feel it in the water'' '
]

{ #category : 'tests - Literal' }
OCScannerTest >> testNextStringContainsAllTheContent [
	| scanner token |
	scanner := self buildScannerForText: '''The world has changed'''.
	token := scanner next.
	self assert: token value equals: 'The world has changed'
]

{ #category : 'tests - Literal' }
OCScannerTest >> testNextStringErrorDoesNotPropagateInside [
	| scanner token |
	scanner := self buildScannerForText: ''' The world has changed ''''I feel it in the water'''.
	token := scanner next.
	self assert: token value equals: ' The world has changed ''I feel it in the water'
]

{ #category : 'tests - Literal' }
OCScannerTest >> testNextStringIsLiteral [
	| scanner token |
	scanner := self buildScannerForText: '''the world has changed'''.
	token := scanner next.
	self assert: token isLiteralToken
]

{ #category : 'tests - Start' }
OCScannerTest >> testNextTokenStartAfterComment [
	"Tests that the next token adapts starting point according to it."
	| scanner token |
	scanner := self buildScannerForText: '"start comment"firstToken'.
	token := scanner next.
	self assert: token value equals: 'firstToken'.
	self assert: token start equals: 16
]

{ #category : 'tests - Start' }
OCScannerTest >> testNextTokenStartAfterLineBreak [
	"Tests that the next token adapts starting point according to it."
	| scanner token |
	scanner := self buildScannerForText: '
firstToken'.
	token := scanner next.
	self assert: token value equals: 'firstToken'.
	self assert: token start equals: 2
]

{ #category : 'tests - Start' }
OCScannerTest >> testNextTokenStartAfterSpaces [
	"Tests that the next token adapts starting point according to it."
	| scanner token |
	scanner := self buildScannerForText: String space, String space, 'firstToken'.
	token := scanner next.
	self assert: token value equals: 'firstToken'.
	self assert: token start equals: 3
]

{ #category : 'tests - Start' }
OCScannerTest >> testNextTokenStartAfterTab [
	"Tests that the next token adapts starting point according to it."
	| scanner token |
	scanner := self buildScannerForText: String tab, 'firstToken'.
	token := scanner next.
	self assert: token value equals: 'firstToken'.
	self assert: token start equals: 2
]

{ #category : 'tests - Separators' }
OCScannerTest >> testNextTrimsBeginningComment [
	| scanner token |
	scanner := self buildScannerForText: '"start comment"firstToken'.
	token := scanner next.
	self assert: token value equals: 'firstToken'
]

{ #category : 'tests - Separators' }
OCScannerTest >> testNextTrimsBeginningLineBreak [
	| scanner token |
	scanner := self buildScannerForText: '
firstToken'.
	token := scanner next.
	self assert: token value equals: 'firstToken'
]

{ #category : 'tests - Separators' }
OCScannerTest >> testNextTrimsBeginningSpaces [
	| scanner token |
	scanner := self buildScannerForText: String space, String space, 'firstToken'.
	token := scanner next.
	self assert: token value equals: 'firstToken'
]

{ #category : 'tests - Separators' }
OCScannerTest >> testNextTrimsBeginningTab [
	| scanner token |
	scanner := self buildScannerForText: String tab, 'firstToken'.
	token := scanner next.
	self assert: token value equals: 'firstToken'
]

{ #category : 'tests - Separators' }
OCScannerTest >> testNextTrimsEndComment [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken"end comment"'.
	scanner next.
	token := scanner next.
	self assert: token isEOF.
	self assert: token start equals: 24
]

{ #category : 'tests - Separators' }
OCScannerTest >> testNextTrimsEndLineBreak [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken
'.
	scanner next.
	token := scanner next.
	self assert: token isEOF.
	self assert: token start equals: 12
]

{ #category : 'tests - Separators' }
OCScannerTest >> testNextTrimsEndSpaces [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken', String space, String space.
	scanner next.
	token := scanner next.
	self assert: token isEOF.
	self assert: token start equals: 13
]

{ #category : 'tests - Separators' }
OCScannerTest >> testNextTrimsEndTab [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken', String tab.
	scanner next.
	token := scanner next.
	self assert: token isEOF.
	self assert: token start equals: 12
]

{ #category : 'tests - Separators' }
OCScannerTest >> testNextTrimsMiddleComment [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken"middle comment"secondToken'.
	scanner next.
	token := scanner next.
	self assert: token isIdentifier.
	self assert: token start equals: 27
]

{ #category : 'tests - Separators' }
OCScannerTest >> testNextTrimsMiddleLineBreak [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken
secondToken'.
	scanner next.
	token := scanner next.
	self assert: token isIdentifier.
	self assert: token start equals: 12
]

{ #category : 'tests - Separators' }
OCScannerTest >> testNextTrimsMiddleSpaces [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken',String space, String space,'secondToken'.
	scanner next.
	token := scanner next.
	self assert: token isIdentifier.
	self assert: token start equals: 13
]

{ #category : 'tests - Separators' }
OCScannerTest >> testNextTrimsMiddleTab [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken', String tab, 'secondToken'.
	scanner next.
	token := scanner next.
	self assert: token isIdentifier.
	self assert: token start equals: 12
]

{ #category : 'tests - Literal' }
OCScannerTest >> testNextTrueIsLiteral [
	| scanner token |
	scanner := self buildScannerForText: 'true'.
	token := scanner next.
	self assert: token isLiteralToken.
	self assert: token value equals: true
]

{ #category : 'tests - Keyword' }
OCScannerTest >> testNextTwoKeywordIsMultipleKeyword [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken:secondToken:'.
	token := scanner next.
	self assert: token isKeyword.
	self assert: token value equals: 'firstToken:'
]

{ #category : 'tests - Identifiers' }
OCScannerTest >> testNextUnderscoreWithOnlyDigitsIsIdentifier [

	"This test shows that the underscore is an alphabetic character."
	| scanner token |
	scanner := self buildScannerForText: '_123'.
	token := scanner next.
	self assert: token isIdentifier.
	self assert: token value equals: '_123'
]

{ #category : 'tests - Error' }
OCScannerTest >> testNextUnknownCharacterThrowsError [
	| scanner token |
	scanner := self buildScannerForText: '`'.
	token := scanner next.
	self assert: token isError.
	self assert: token value equals: '`'.
	self assert: token cause equals: 'Unknown character'
]

{ #category : 'tests - NumberLiteral' }
OCScannerTest >> testNextValueOfNegativeNumbers [

	| scanner token |
	scanner := self buildScannerForText: '-12'.
	token := scanner next.
	self assert: token value equals: -12.

	scanner := self buildScannerForText: '-3'.
	token := scanner next.
	self assert: token value equals: -3
]

{ #category : 'tests - NumberLiteral' }
OCScannerTest >> testNextValueOfRadixNumbersCutsWhenValueIsImpossible [

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
OCScannerTest >> testNextValueOfRadixNumbersIsConvertedToDecimal [

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
OCScannerTest >> testNextWithAWrongSymbolGetError [
	| source scanner scannedToken |
	source := '#^'.
	scanner := self buildScannerForText: source.
	scannedToken := scanner next.
	self verifyErrorToken: scannedToken message: 'Literal expected' translated valueExpected: '#'
]

{ #category : 'tests' }
OCScannerTest >> testNextWithAnIdentifierTokenGetTheIdentifierToken [
	| source scanner token |
	source := 'identifierToken'.
	scanner := self buildScannerForText: source.
	token := scanner next.
	self assert: token isIdentifier.
	self assert: token value equals: source
]

{ #category : 'tests' }
OCScannerTest >> testNextWithAnOpeningCommentAtEndGetError [
	| source token |
	source := 'self "'.
	token := (self buildScannerForText: source) next; next.
	self assert: token isError.
	self assert: token cause equals: 'Unmatched " in comment.'.
	self assert: token value equals: '"'
]

{ #category : 'tests' }
OCScannerTest >> testNextWithAnOpeningCommentGetError [
	| source token |
	source := '"only the opening'.
	token := (self buildScannerForText: source) next.
	self assert: token isError.
	self assert: token cause equals: 'Unmatched " in comment.'.
	self assert: token value equals: source
]

{ #category : 'tests' }
OCScannerTest >> testNextWithAnOpeningStringsGetError [
	| source scannedToken |
	source := '''only the opening'.
	scannedToken := (self buildScannerForText: source) next.
	self verifyErrorToken: scannedToken message: 'Unmatched '' in string literal.' translated valueExpected: source
]

{ #category : 'tests' }
OCScannerTest >> testNextWithAnUnknownCharacterGetError [
	| source scanner scannedToken |
	source := '¿'.
	scanner := self buildScannerForText: source.
	scannedToken := scanner next.
	self verifyErrorToken: scannedToken message: 'Unknown character' translated valueExpected: source
]

{ #category : 'tests - Comment' }
OCScannerTest >> testNextWithTwoDoubleQuotesInComment [
	| source token |
	source := '"only the"" opening"'.
	self
		shouldnt: [ token := (self buildScannerForText: source) next ]
		raise: OCCodeError.
	self assert: token comments first value equals: 'only the" opening'
]

{ #category : 'tests' }
OCScannerTest >> testNextWithTwoDoubleQuotesInCommentGetError [
	| source token |
	source := '"only the"" opening'.
	token := (self buildScannerForText: source) next.
	self assert: token isError.
	self assert: token cause equals: 'Unmatched " in comment.'.
	self assert: token value equals: source
]

{ #category : 'tests - Error' }
OCScannerTest >> testNextWithUnFinishedCommentGivesAnError [

	| source token |
	source := 'firstToken "unfinished comment secondToken'.
	token := (self buildScannerForText: source) next; next.
	self assert: token isError.
	self assert: token cause equals: 'Unmatched " in comment.'.
	self assert: token value equals: '"unfinished comment secondToken'
]

{ #category : 'tests - Creation api' }
OCScannerTest >> testScanTokenObjects1 [

	 | inp exp |
	 inp := 'Object subclass: #NameOfSubclass'.
	 exp := {
		        'Object'.
		        'subclass:'.
		        #NameOfSubclass asString }.
	 self
		 assert:
		 ((self scannerClass scanTokens: inp) collect: [ :each | each value ])
		 equals: exp
]

{ #category : 'tests - Creation api' }
OCScannerTest >> testScanTokenObjects2 [

	 | inp exp |
	 inp := 'classVariableNames: '''' "ha ha"
package: ''UndefinedClasses-Experiment'.
	 exp := { 'classVariableNames:'. ''. 'package:'. '''UndefinedClasses-Experiment' }.
	 self
		 assert:
		 ((self scannerClass scanTokens: inp) collect: [ :each | each value ])
		 equals: exp
]

{ #category : 'tests - ScanTokens' }
OCScannerTest >> testScanTokensDoesNotBreak [

	"Pay attention the scanToken instance side method is brittle and it can break 
	with simple code such as
	
	| tok |
tok := OrderedCollection new. 
scan := RBScanner on: 'end - level' readStream.
[ scan atEnd ] whileFalse: [ 
	tok add: scan scanToken ].
tok 
	
	While the same is working with scan next
	"

	"Now this test is validating the class side method scanTokens"
	
	| tokens |
	tokens := OCScanner scanTokens: 'end - level'.
	self assert: tokens size equals: 3.
	self assert: tokens first value equals: 'end'.
	self assert: tokens second value equals: #-.
	self assert: tokens third value equals: 'level'.
	
]

{ #category : 'tests - ScanTokens' }
OCScannerTest >> testScanTokensHandleParentheses [
	
	| tokens |
	tokens := OCScanner scanTokens: '( end - level ) '.
	self assert: tokens size equals: 5.
	self assert: tokens first value equals: $(.
	self assert: tokens second value equals: 'end'.
	self assert: tokens fifth value equals: $).
	
]

{ #category : 'utilities' }
OCScannerTest >> verifyErrorToken: scannedToken message: message valueExpected: valueExpected [
	self assert: scannedToken isError.
	self assert: scannedToken cause equals: message.
	self assert: scannedToken value equals: valueExpected.
	self assert: scannedToken start equals: 1
]
