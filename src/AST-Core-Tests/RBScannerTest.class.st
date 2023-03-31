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
	^ self scannerClass on: source readStream
]

{ #category : #initialize }
RBScannerTest >> scannerClass [
	^ RBScanner
]

{ #category : #'tests - Comment' }
RBScannerTest >> testCommentTokenHasTheRightValue [
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

{ #category : #'tests - Comment' }
RBScannerTest >> testCommentTokenStartAtFirstQuote [
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

{ #category : #'tests - Comment' }
RBScannerTest >> testCommentTokenStopAtLastQuote [
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

{ #category : #'tests - Comment' }
RBScannerTest >> testMultipleCommentTokensAreWellSeparated [
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

{ #category : #'tests - Literal' }
RBScannerTest >> testNextAcceptAllAlphabeticAsLiteral [
	| scanner token |
	scanner := self buildScannerForText: '#alpha'.
	token := scanner next.
	self assert: token isLiteralToken
]

{ #category : #'tests - NumberLiteral' }
RBScannerTest >> testNextAcceptFloatNumbers [

	| scanner token |
	scanner := self buildScannerForText: '3.2'.
	token := scanner next.
	self assert: token isNumberLiteralToken
]

{ #category : #'tests - SpecialCharacters' }
RBScannerTest >> testNextAcceptLiteralArrayWithBracket [
	"Note: RBScanner only spots the beginning of the array."
	| scanner token |

	scanner := self buildScannerForText: '#['.
	token := scanner next.
	self assert: token isLiteralArrayToken
]

{ #category : #'tests - SpecialCharacters' }
RBScannerTest >> testNextAcceptLiteralArrayWithParenthesis [
	"Note: RBScanner only spots the beginning of the array."
	| scanner token |
	scanner := self buildScannerForText: '#('.
	token := scanner next.
	self assert: token isLiteralArrayToken
]

{ #category : #'tests - NumberLiteral' }
RBScannerTest >> testNextAcceptRadixNumbers [

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

{ #category : #'tests - Keyword' }
RBScannerTest >> testNextAllAlphabeticEndingWithColonGiveKeyword [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken:'.
	token := scanner next.
	self assert: token isKeyword
]

{ #category : #'tests - Identifiers' }
RBScannerTest >> testNextAllAlphabeticsGiveIdentifier [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken'.
	token := scanner next.
	self assert: token isIdentifier
]

{ #category : #'tests - Keyword' }
RBScannerTest >> testNextAlphabeticAndDigitEndingWithColonGiveKeyword [
	| scanner token |
	scanner := self buildScannerForText: 'first123Token:'.
	token := scanner next.
	self assert: token isKeyword
]

{ #category : #'tests - Identifiers' }
RBScannerTest >> testNextAlphabeticsAndDigitGiveIdentifier [
	| scanner token |
	scanner := self buildScannerForText: 'first123Token'.
	token := scanner next.
	self assert: token isIdentifier
]

{ #category : #'tests - Start' }
RBScannerTest >> testNextAssignementStartAtColon [
	| scanner token |
	scanner := self buildScannerForText: ':='.
	token := scanner next.
	self assert: token start equals: 1
]

{ #category : #'tests - Stop' }
RBScannerTest >> testNextAssignementStopAtEqual [
	| scanner token |
	scanner := self buildScannerForText: ':='.
	token := scanner next.
	self assert: token stop equals: 2
]

{ #category : #'tests - Error' }
RBScannerTest >> testNextBaseOfRadixNumbersCanNotBeOne [

	| scanner token |
	scanner := self buildScannerForText: '1r1a'.
	token := scanner next.
	self assert: token isError.
	self assert: token start equals: 1
]

{ #category : #'tests - NumberLiteral' }
RBScannerTest >> testNextBeginningWithDigitIsNumber [

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

{ #category : #'tests - BinarySelector' }
RBScannerTest >> testNextBinaryGivesBinarySelector [
	| scanner token |
	scanner := self buildScannerForText: '+'.
	token := scanner next.
	self assert: token isBinary
]

{ #category : #'tests - BinarySelector' }
RBScannerTest >> testNextBinarySelectorCanBeMultipleBinaries [
	| scanner token |
	scanner := self buildScannerForText: '+-=/'.
	token := scanner next.
	self assert: token isBinary.
	self assert: token value equals: '+-=/'
]

{ #category : #'tests - Start' }
RBScannerTest >> testNextBinarySelectorStartAtFirstLetter [
	| scanner token |
	scanner := self buildScannerForText: '+-=/'.
	token := scanner next.
	self assert: token value equals: '+-=/'.
	self assert: token start equals: 1
]

{ #category : #'tests - Stop' }
RBScannerTest >> testNextBinarySelectorStopAtLastLetter [
	| scanner token |
	scanner := self buildScannerForText: '+-=/'.
	token := scanner next.
	self assert: token value equals: '+-=/'.
	self assert: token stop equals: 4
]

{ #category : #'tests - BinarySelector' }
RBScannerTest >> testNextBinarySelectorStopsAtNonBinary [
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

{ #category : #'tests - BinarySelector' }
RBScannerTest >> testNextBinarySelectorValueIsBinary [
	| scanner token |
	scanner := self buildScannerForText: '+'.
	token := scanner next.
	self assert: token value equals: '+'
]

{ #category : #'tests - Comment' }
RBScannerTest >> testNextCommentIsCommentToken [
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

{ #category : #'tests - Separators' }
RBScannerTest >> testNextDoesNotAddLineBreakToComments [
	| scanner token |
	scanner := self buildScannerForText: '
firstToken'.
	token := scanner next.
	self assert: token comments isNil
]

{ #category : #'tests - Separators' }
RBScannerTest >> testNextDoesNotAddSpacesToComments [
	| scanner token |
	scanner := self buildScannerForText: String space, String space,'firstToken'.
	token := scanner next.
	self assert: token comments isNil
]

{ #category : #'tests - Separators' }
RBScannerTest >> testNextDoesNotAddTabToComments [
	| scanner token |
	scanner := self buildScannerForText: String tab, 'firstToken'.
	token := scanner next.
	self assert: token comments isNil
]

{ #category : #'tests - Error' }
RBScannerTest >> testNextDollarSignErrorHasDollarValue [
	| scanner token |
	scanner := self buildScannerForText: '$'.
	token := scanner next.
	self assert: token value equals: '$'
]

{ #category : #'tests - Error' }
RBScannerTest >> testNextDollarSignExpectedACharacter [
	| scanner token |
	scanner := self buildScannerForText: '$'.
	token := scanner next.
	self assert: token cause equals: 'Character expected'
]

{ #category : #'tests - Error' }
RBScannerTest >> testNextDollarSignIsError [
	| scanner token |
	scanner := self buildScannerForText: '$'.
	token := scanner next.
	self assert: token isError
]

{ #category : #'tests - Separators' }
RBScannerTest >> testNextEOFContainsCommentsWhenNoOtherToken [
	| scanner token |
	scanner := self buildScannerForText: '"start comment"'.

	token := scanner next.
	self assert: token isEOF.
	self assert: token comments size equals: 1.
	self assert: token start equals: 16
]

{ #category : #'tests - Start' }
RBScannerTest >> testNextErrorStartAtFirstCharacter [
	| scanner token |
	scanner := self buildScannerForText: '$'.
	token := scanner next.
	self assert: token start equals: 1.

	scanner := self buildScannerForText: '#123alpha'.
	token := scanner next.
	self assert: token isError.
	self assert: token start equals: 1
]

{ #category : #'tests - Stop' }
RBScannerTest >> testNextErrorStopAtErrorCharacter [
	| scanner token |
	scanner := self buildScannerForText: '$'.
	token := scanner next.
	self assert: token stop equals: 1.

	scanner := self buildScannerForText: '#123alpha'.
	token := scanner next.
	self assert: token isError.
	self assert: token stop equals: 1
]

{ #category : #'tests - Literal' }
RBScannerTest >> testNextFalseIsLiteral [
	| scanner token |
	scanner := self buildScannerForText: 'false'.
	token := scanner next.
	self assert: token isLiteralToken.
	self assert: token value equals: false
]

{ #category : #'tests - Separators' }
RBScannerTest >> testNextFirstTokenContainsFollowingComments [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken "following comment"'.

	"The first token 'firstToken' takes the previous comment and the following one"
	token := scanner next.
	self assert: token comments size equals: 1.
	self assert: token comments first isComment
]

{ #category : #'tests - Separators' }
RBScannerTest >> testNextFirstTokenContainsPrecedingComments [
	| scanner token |
	scanner := self buildScannerForText: '"start comment"firstToken'.
	token := scanner next.
	self assert: token comments size equals: 1.
	self assert: token comments first isComment
]

{ #category : #'tests - NumberLiteral' }
RBScannerTest >> testNextFloatNumbersCanBeNegative [

	| scanner token |
	scanner := self buildScannerForText: '-3.2'.
	token := scanner next.
	self assert: token value equals: -3.2
]

{ #category : #'tests - NumberLiteral' }
RBScannerTest >> testNextFloatNumbersCanNotHaveSpaces [

	| scanner token |
	scanner := self buildScannerForText: '3 .2'.
	token := scanner next.
	self assert: token value equals: 3.

	scanner := self buildScannerForText: '3. 2'.
	token := scanner next.
	self assert: token value equals: 3
]

{ #category : #'tests - NumberLiteral' }
RBScannerTest >> testNextFloatNumbersHasFloatValue [

	| scanner token |
	scanner := self buildScannerForText: '3.2'.
	token := scanner next.
	self assert: token value equals: 3.2
]

{ #category : #'tests - Error' }
RBScannerTest >> testNextHashtagCanNotStandAlone [
	| scanner token |
	scanner := self buildScannerForText: '#'.
	token := scanner next.
	self assert: token isError
]

{ #category : #'tests - Literal' }
RBScannerTest >> testNextHashtagStringIsLiteral [
	| scanner token |
	scanner := self buildScannerForText: '#''the world has changed'''.
	token := scanner next.
	self assert: token isLiteralToken.
	self assert: token value equals: #'the world has changed'
]

{ #category : #'tests - Identifiers' }
RBScannerTest >> testNextIdentifierCanContainUnderscore [
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

{ #category : #'tests - Identifiers' }
RBScannerTest >> testNextIdentifierCanNotBeginWithBinary [
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

{ #category : #'tests - Identifiers' }
RBScannerTest >> testNextIdentifierCanNotBeginWithDigit [
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

{ #category : #'tests - Identifiers' }
RBScannerTest >> testNextIdentifierCanNotEndWithBinary [
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

{ #category : #'tests - Identifiers' }
RBScannerTest >> testNextIdentifierCanNotHaveBinary [
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

{ #category : #'tests - Start' }
RBScannerTest >> testNextIdentifierStartAtFirstLetter [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken'.
	token := scanner next.
	self assert: token start equals: 1
]

{ #category : #'tests - Stop' }
RBScannerTest >> testNextIdentifierStopAtLastLetter [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken'.
	token := scanner next.
	self assert: token stop equals: 10
]

{ #category : #'tests - Identifiers' }
RBScannerTest >> testNextIdentifierValueIsName [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken'.
	token := scanner next.
	self assert: token value equals: 'firstToken'
]

{ #category : #'tests - Start' }
RBScannerTest >> testNextKeyWordStartAtFirstLetter [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken:'.
	token := scanner next.
	self assert: token start equals: 1
]

{ #category : #'tests - Keyword' }
RBScannerTest >> testNextKeywordBeginningWithSpecialCharacterIsNotKeyword [
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

{ #category : #'tests - Keyword' }
RBScannerTest >> testNextKeywordCanContainUnderscore [
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

{ #category : #'tests - Keyword' }
RBScannerTest >> testNextKeywordCanNotBeFullBinary [
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

{ #category : #'tests - Keyword' }
RBScannerTest >> testNextKeywordCanNotBeFullDigit [
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

{ #category : #'tests - Keyword' }
RBScannerTest >> testNextKeywordCanNotBeginWithBinary [
	| scanner token |
	scanner := self buildScannerForText: '+alpha:'.

	token := scanner next.
	self assert: token isBinary.
	self assert: token value equals: '+'.

	token := scanner next.
	self assert: token isKeyword.
	self assert: token value equals: 'alpha:'
]

{ #category : #'tests - Keyword' }
RBScannerTest >> testNextKeywordCanNotBeginWithDigit [
	| scanner token |
	scanner := self buildScannerForText: '123alpha:'.

	token := scanner next.
	self assert: token isNumberLiteralToken.
	self assert: token value equals: 123.

	token := scanner next.
	self assert: token isKeyword.
	self assert: token value equals: 'alpha:'
]

{ #category : #'tests - Stop' }
RBScannerTest >> testNextKeywordStopAtColon [
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

{ #category : #'tests - Keyword' }
RBScannerTest >> testNextKeywordValueIsStartToStop [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken:'.
	token := scanner next.
	self assert: token value equals: 'firstToken:'.

	scanner := self buildScannerForText: 'first:Token'.
	token := scanner next.
	self assert: token value equals: 'first:'
]

{ #category : #'tests - SpecialCharacters' }
RBScannerTest >> testNextLiteralArrayHasRightValue [
	"Note: RBScanner only spots the beginning of the array."
	| scanner token |

	scanner := self buildScannerForText: '#['.
	token := scanner next.
	self assert: token value equals: '#['
]

{ #category : #'tests - Start' }
RBScannerTest >> testNextLiteralArrayWithBracketStartAtHashtag [
	"Note: RBScanner only spots the beginning of the array."
	| scanner token |

	scanner := self buildScannerForText: '#['.
	token := scanner next.
	self assert: token start equals: 1
]

{ #category : #'tests - Stop' }
RBScannerTest >> testNextLiteralArrayWithBracketStopAtBracket [
	"Note: RBScanner only spots the beginning of the array."
	| scanner token |

	scanner := self buildScannerForText: '#['.
	token := scanner next.
	self assert: token stop equals: 2
]

{ #category : #'tests - SpecialCharacters' }
RBScannerTest >> testNextLiteralArrayWithParenthesisHasRigthValue [
	"Note: RBScanner only spots the beginning of the array."
	| scanner token |
	scanner := self buildScannerForText: '#('.
	token := scanner next.
	self assert: token value equals: '#('
]

{ #category : #'tests - Start' }
RBScannerTest >> testNextLiteralArrayWithParenthesisStartAtHashTag [
	"Note: RBScanner only spots the beginning of the array."
	| scanner token |
	scanner := self buildScannerForText: '#('.
	token := scanner next.
	self assert: token start equals: 1
]

{ #category : #'tests - Stop' }
RBScannerTest >> testNextLiteralArrayWithParenthesisStopAtParenthesis [
	"Note: RBScanner only spots the beginning of the array."
	| scanner token |
	scanner := self buildScannerForText: '#('.
	token := scanner next.
	self assert: token stop equals: 2
]

{ #category : #'tests - Error' }
RBScannerTest >> testNextLiteralBeginningWithApostropheThrowsError [
	| scanner token |
	scanner := self buildScannerForText: '#'''.
	token := scanner next.
	self assert: token isError.
	self assert: token value equals: #'#'''.
	self assert: token cause equals: 'Unmatched '' in string literal.'
]

{ #category : #'tests - Error' }
RBScannerTest >> testNextLiteralBeginningWithDigitThrowsError [
	| scanner token |
	scanner := self buildScannerForText: '#123alpha'.
	token := scanner next.
	self assert: token isError.
	self assert: token value equals: '#'.
	self assert: token cause equals: 'Literal expected'
]

{ #category : #'tests - Error' }
RBScannerTest >> testNextLiteralBeginningWithSpecialThrowsError [
	| scanner token |
	scanner := self buildScannerForText: '#^'.
	token := scanner next.
	self assert: token isError.
	self assert: token value equals: '#'.
	self assert: token cause equals: 'Literal expected'
]

{ #category : #'tests - Error' }
RBScannerTest >> testNextLiteralBeginningWithUnknownThrowsError [
	| scanner token |
	scanner := self buildScannerForText: '#`'.
	token := scanner next.
	self assert: token isError.
	self assert: token value equals: '#'.
	self assert: token cause equals: 'Literal expected'
]

{ #category : #'tests - Literal' }
RBScannerTest >> testNextLiteralCanBeBinary [
	| scanner token |
	scanner := self buildScannerForText: '#+=-/'.
	token := scanner next.
	self assert: token isLiteralToken.
	self assert: token value equals: #+=-/
]

{ #category : #'tests - Literal' }
RBScannerTest >> testNextLiteralCanContainColon [
	| scanner token |

	scanner := self buildScannerForText: '#al:pha'.
	token := scanner next.
	self assert: token isLiteralToken.
	self assert: token value equals: #'al:pha'
]

{ #category : #'tests - Literal' }
RBScannerTest >> testNextLiteralCanContainDigits [
	| scanner token |
	scanner := self buildScannerForText: '#alpha123beta456'.
	token := scanner next.
	self assert: token isLiteralToken.
	self assert: token value equals: #alpha123beta456
]

{ #category : #'tests - Literal' }
RBScannerTest >> testNextLiteralCanHaveMultipleHashtag [
	| scanner token |
	scanner := self buildScannerForText: '########literal'.
	token := scanner next.
	self assert: token isLiteralToken
]

{ #category : #'tests - Literal' }
RBScannerTest >> testNextLiteralCanNotContainBinaryAndAlphabetic [
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

{ #category : #'tests - Literal' }
RBScannerTest >> testNextLiteralCharacterCanBeAnyCharacter [
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

{ #category : #'tests - Literal' }
RBScannerTest >> testNextLiteralCharacterHasCharacterAsValue [
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

{ #category : #'tests - Literal' }
RBScannerTest >> testNextLiteralCharacterHasOnlyOneCharacter [
	| scanner token |
	scanner := self buildScannerForText: '$123'.
	token := scanner next.
	self assert: token value equals: $1
]

{ #category : #tests }
RBScannerTest >> testNextLiteralCharacterWithAMissingCharacter [
	| source scanner scannedToken |
	source := '$'.
	scanner := self buildScannerForText: source.
	scannedToken := scanner next.
	self verifyErrorToken: scannedToken message: 'Character expected' translated valueExpected: '$'
]

{ #category : #'tests - Start' }
RBScannerTest >> testNextLiteralStartAtFirstLetter [
	| scanner token |
	scanner := self buildScannerForText: '########literal'.
	token := scanner next.
	self assert: token isLiteralToken.
	self assert: token start equals: 1
]

{ #category : #'tests - Stop' }
RBScannerTest >> testNextLiteralStopAtLastLetter [
	| scanner token |
	scanner := self buildScannerForText: '########literal'.
	token := scanner next.
	self assert: token isLiteralToken.
	self assert: token stop equals: 15
]

{ #category : #'tests - Literal' }
RBScannerTest >> testNextLiteralWithMultipleHashtagTrimThem [
	| scanner token |
	scanner := self buildScannerForText: '########literal'.
	token := scanner next.
	self assert: token value equals: 'literal'
]

{ #category : #'tests - Keyword' }
RBScannerTest >> testNextMultipleKeywordColonIsSufficientAfterFirstKeyword [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken::'.
	token := scanner next.
	self assert: token isMultiKeyword.
	self assert: token value equals: 'firstToken::'.

	scanner := self buildScannerForText: 'firstToken::::::'.
	token := scanner next.
	self assert: token isMultiKeyword.
	self assert: token value equals: 'firstToken::::::'
]

{ #category : #'tests - Start' }
RBScannerTest >> testNextMultipleKeywordStartAtFirstLetter [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken:secondToken:'.
	token := scanner next.
	self assert: token isMultiKeyword.
	self assert: token start equals: 1
]

{ #category : #'tests - Stop' }
RBScannerTest >> testNextMultipleKeywordStopAtLastLetter [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken:secondToken:'.
	token := scanner next.
	self assert: token isMultiKeyword.
	self assert: token stop equals: 23
]

{ #category : #'tests - NumberLiteral' }
RBScannerTest >> testNextNegativeFloatNumbersIsNumberLiteral [

	| scanner token |
	scanner := self buildScannerForText: '-3.2'.
	token := scanner next.
	self assert: token isNumberLiteralToken
]

{ #category : #'tests - NumberLiteral' }
RBScannerTest >> testNextNegativeNumberCanNotHaveSpaces [

	| scanner token |
	scanner := self buildScannerForText: '- 12'.

	token := scanner next.
	self assert: token isBinary.
	self assert: token value equals: '-'.

	token := scanner next.
	self assert: token isNumberLiteralToken.
	self assert: token value equals: 12
]

{ #category : #'tests - NumberLiteral' }
RBScannerTest >> testNextNegativeNumberIsNumberLiteral [

	| scanner token |
	scanner := self buildScannerForText: '-12'.
	token := scanner next.
	self assert: token isNumberLiteralToken
]

{ #category : #'tests - Literal' }
RBScannerTest >> testNextNilIsLiteral [
	| scanner token |
	scanner := self buildScannerForText: 'nil'.
	token := scanner next.
	self assert: token isLiteralToken.
	self assert: token value equals: nil
]

{ #category : #'tests - Start' }
RBScannerTest >> testNextNumberLiteralStartAtTheBeginning [

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

{ #category : #'tests - Stop' }
RBScannerTest >> testNextNumberLiteralStopAtTheLastDigit [

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

{ #category : #'tests - NumberLiteral' }
RBScannerTest >> testNextRadixNumbersCanBeFloatAsWell [

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

{ #category : #'tests - NumberLiteral' }
RBScannerTest >> testNextRadixNumbersCanBeNegative [

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

{ #category : #'tests - Separators' }
RBScannerTest >> testNextSecondTokenDoesNotConsumePrecedingComment [
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

{ #category : #'tests - Start' }
RBScannerTest >> testNextSpecialCharacterStartAtTheBeginning [
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

{ #category : #'tests - Stop' }
RBScannerTest >> testNextSpecialCharacterStopAtThemself [
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

{ #category : #'tests - SpecialCharacters' }
RBScannerTest >> testNextSpecialCharactereCanBeAssignement [
	| scanner token |
	scanner := self buildScannerForText: ':='.
	token := scanner next.
	self assert: token start equals: 1
]

{ #category : #'tests - SpecialCharacters' }
RBScannerTest >> testNextSpecialCharactereisSingle [
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

{ #category : #'tests - SpecialCharacters' }
RBScannerTest >> testNextSpecialCharactereisSpecial [
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

{ #category : #'tests - Literal' }
RBScannerTest >> testNextStringCanContainOtherString [
	| scanner token |
	scanner := self buildScannerForText: ''' The world has changed ''''I can feel it in the water'''' '''.
	token := scanner next.
	self assert: token isLiteralToken.
	self assert: token value equals: ' The world has changed ''I can feel it in the water'' '
]

{ #category : #'tests - Literal' }
RBScannerTest >> testNextStringContainsAllTheContent [
	| scanner token |
	scanner := self buildScannerForText: '''The world has changed'''.
	token := scanner next.
	self assert: token value equals: 'The world has changed'
]

{ #category : #'tests - Literal' }
RBScannerTest >> testNextStringErrorDoesNotPropagateInside [
	| scanner token |
	scanner := self buildScannerForText: ''' The world has changed ''''I feel it in the water'''.
	token := scanner next.
	self assert: token value equals: ' The world has changed ''I feel it in the water'
]

{ #category : #'tests - Literal' }
RBScannerTest >> testNextStringIsLiteral [
	| scanner token |
	scanner := self buildScannerForText: '''the world has changed'''.
	token := scanner next.
	self assert: token isLiteralToken
]

{ #category : #'tests - Start' }
RBScannerTest >> testNextTokenStartAfterComment [
	"Tests that the next token adapts starting point according to it."
	| scanner token |
	scanner := self buildScannerForText: '"start comment"firstToken'.
	token := scanner next.
	self assert: token value equals: 'firstToken'.
	self assert: token start equals: 16
]

{ #category : #'tests - Start' }
RBScannerTest >> testNextTokenStartAfterLineBreak [
	"Tests that the next token adapts starting point according to it."
	| scanner token |
	scanner := self buildScannerForText: '
firstToken'.
	token := scanner next.
	self assert: token value equals: 'firstToken'.
	self assert: token start equals: 2
]

{ #category : #'tests - Start' }
RBScannerTest >> testNextTokenStartAfterSpaces [
	"Tests that the next token adapts starting point according to it."
	| scanner token |
	scanner := self buildScannerForText: String space, String space, 'firstToken'.
	token := scanner next.
	self assert: token value equals: 'firstToken'.
	self assert: token start equals: 3
]

{ #category : #'tests - Start' }
RBScannerTest >> testNextTokenStartAfterTab [
	"Tests that the next token adapts starting point according to it."
	| scanner token |
	scanner := self buildScannerForText: String tab, 'firstToken'.
	token := scanner next.
	self assert: token value equals: 'firstToken'.
	self assert: token start equals: 2
]

{ #category : #'tests - Separators' }
RBScannerTest >> testNextTrimsBeginningComment [
	| scanner token |
	scanner := self buildScannerForText: '"start comment"firstToken'.
	token := scanner next.
	self assert: token value equals: 'firstToken'
]

{ #category : #'tests - Separators' }
RBScannerTest >> testNextTrimsBeginningLineBreak [
	| scanner token |
	scanner := self buildScannerForText: '
firstToken'.
	token := scanner next.
	self assert: token value equals: 'firstToken'
]

{ #category : #'tests - Separators' }
RBScannerTest >> testNextTrimsBeginningSpaces [
	| scanner token |
	scanner := self buildScannerForText: String space, String space, 'firstToken'.
	token := scanner next.
	self assert: token value equals: 'firstToken'
]

{ #category : #'tests - Separators' }
RBScannerTest >> testNextTrimsBeginningTab [
	| scanner token |
	scanner := self buildScannerForText: String tab, 'firstToken'.
	token := scanner next.
	self assert: token value equals: 'firstToken'
]

{ #category : #'tests - Separators' }
RBScannerTest >> testNextTrimsEndComment [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken"end comment"'.
	scanner next.
	token := scanner next.
	self assert: token isEOF.
	self assert: token start equals: 24
]

{ #category : #'tests - Separators' }
RBScannerTest >> testNextTrimsEndLineBreak [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken
'.
	scanner next.
	token := scanner next.
	self assert: token isEOF.
	self assert: token start equals: 12
]

{ #category : #'tests - Separators' }
RBScannerTest >> testNextTrimsEndSpaces [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken', String space, String space.
	scanner next.
	token := scanner next.
	self assert: token isEOF.
	self assert: token start equals: 13
]

{ #category : #'tests - Separators' }
RBScannerTest >> testNextTrimsEndTab [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken', String tab.
	scanner next.
	token := scanner next.
	self assert: token isEOF.
	self assert: token start equals: 12
]

{ #category : #'tests - Separators' }
RBScannerTest >> testNextTrimsMiddleComment [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken"middle comment"secondToken'.
	scanner next.
	token := scanner next.
	self assert: token isIdentifier.
	self assert: token start equals: 27
]

{ #category : #'tests - Separators' }
RBScannerTest >> testNextTrimsMiddleLineBreak [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken
secondToken'.
	scanner next.
	token := scanner next.
	self assert: token isIdentifier.
	self assert: token start equals: 12
]

{ #category : #'tests - Separators' }
RBScannerTest >> testNextTrimsMiddleSpaces [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken',String space, String space,'secondToken'.
	scanner next.
	token := scanner next.
	self assert: token isIdentifier.
	self assert: token start equals: 13
]

{ #category : #'tests - Separators' }
RBScannerTest >> testNextTrimsMiddleTab [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken', String tab, 'secondToken'.
	scanner next.
	token := scanner next.
	self assert: token isIdentifier.
	self assert: token start equals: 12
]

{ #category : #'tests - Literal' }
RBScannerTest >> testNextTrueIsLiteral [
	| scanner token |
	scanner := self buildScannerForText: 'true'.
	token := scanner next.
	self assert: token isLiteralToken.
	self assert: token value equals: true
]

{ #category : #'tests - Keyword' }
RBScannerTest >> testNextTwoKeywordIsMultipleKeyword [
	| scanner token |
	scanner := self buildScannerForText: 'firstToken:secondToken:'.
	token := scanner next.
	self assert: token isMultiKeyword.
	self assert: token value equals: 'firstToken:secondToken:'
]

{ #category : #'tests - Identifiers' }
RBScannerTest >> testNextUnderscoreWithOnlyDigitsIsIdentifier [

	"This test shows that the underscore is an alphabetic character."
	| scanner token |
	scanner := self buildScannerForText: '_123'.
	token := scanner next.
	self assert: token isIdentifier.
	self assert: token value equals: '_123'
]

{ #category : #'tests - Error' }
RBScannerTest >> testNextUnknownCharacterThrowsError [
	| scanner token |
	scanner := self buildScannerForText: '`'.
	token := scanner next.
	self assert: token isError.
	self assert: token value equals: '`'.
	self assert: token cause equals: 'Unknown character'
]

{ #category : #'tests - NumberLiteral' }
RBScannerTest >> testNextValueOfNegativeNumbers [

	| scanner token |
	scanner := self buildScannerForText: '-12'.
	token := scanner next.
	self assert: token value equals: -12.

	scanner := self buildScannerForText: '-3'.
	token := scanner next.
	self assert: token value equals: -3
]

{ #category : #'tests - NumberLiteral' }
RBScannerTest >> testNextValueOfRadixNumbersCutsWhenValueIsImpossible [

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

{ #category : #'tests - NumberLiteral' }
RBScannerTest >> testNextValueOfRadixNumbersIsConvertedToDecimal [

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

{ #category : #tests }
RBScannerTest >> testNextWithAWrongSymbolGetError [
	| source scanner scannedToken |
	source := '#^'.
	scanner := self buildScannerForText: source.
	scannedToken := scanner next.
	self verifyErrorToken: scannedToken message: 'Literal expected' translated valueExpected: '#'
]

{ #category : #tests }
RBScannerTest >> testNextWithAnIdentifierTokenGetTheIdentifierToken [
	| source scanner token |
	source := 'identifierToken'.
	scanner := self buildScannerForText: source.
	token := scanner next.
	self assert: token isIdentifier.
	self assert: token value equals: source
]

{ #category : #tests }
RBScannerTest >> testNextWithAnOpeningCommentAtEndGetError [
	| source token |
	source := 'self "'.
	token := (self buildScannerForText: source) next; next.
	self assert: token isError.
	self assert: token cause equals: 'Unmatched " in comment.'.
	self assert: token value equals: '"'
]

{ #category : #tests }
RBScannerTest >> testNextWithAnOpeningCommentGetError [
	| source token |
	source := '"only the opening'.
	token := (self buildScannerForText: source) next.
	self assert: token isError.
	self assert: token cause equals: 'Unmatched " in comment.'.
	self assert: token value equals: source
]

{ #category : #tests }
RBScannerTest >> testNextWithAnOpeningStringsGetError [
	| source scannedToken |
	source := '''only the opening'.
	scannedToken := (self buildScannerForText: source) next.
	self verifyErrorToken: scannedToken message: 'Unmatched '' in string literal.' translated valueExpected: source
]

{ #category : #tests }
RBScannerTest >> testNextWithAnUnknownCharacterGetError [
	| source scanner scannedToken |
	source := '¿'.
	scanner := self buildScannerForText: source.
	scannedToken := scanner next.
	self verifyErrorToken: scannedToken message: 'Unknown character' translated valueExpected: source
]

{ #category : #'tests - Comment' }
RBScannerTest >> testNextWithTwoDoubleQuotesInComment [
	| source token |
	source := '"only the"" opening"'.
	self
		shouldnt: [ token := (self buildScannerForText: source) next ]
		raise: SyntaxErrorNotification.
	self assert: token comments first value equals: 'only the" opening'
]

{ #category : #tests }
RBScannerTest >> testNextWithTwoDoubleQuotesInCommentGetError [
	| source token |
	source := '"only the"" opening'.
	token := (self buildScannerForText: source) next.
	self assert: token isError.
	self assert: token cause equals: 'Unmatched " in comment.'.
	self assert: token value equals: source
]

{ #category : #'tests - Error' }
RBScannerTest >> testNextWithUnFinishedCommentGivesAnError [

	| source token |
	source := 'firstToken "unfinished comment secondToken'.
	token := (self buildScannerForText: source) next; next.
	self assert: token isError.
	self assert: token cause equals: 'Unmatched " in comment.'.
	self assert: token value equals: '"unfinished comment secondToken'
]

{ #category : #'tests - Creation api' }
RBScannerTest >> testScanTokenObjects1 [
	| inp exp |
	inp := 'Object subclass: #NameOfSubclass'.
	exp := {'Object'.
	'subclass:'.
	#NameOfSubclass asString}.
	self assert: ((self scannerClass scanTokenObjects: inp) collect: [ :each | each value ]) equals: exp
]

{ #category : #'tests - Creation api' }
RBScannerTest >> testScanTokenObjects2 [
	| inp exp |
	inp := 'classVariableNames: '''' "ha ha"
package: ''UndefinedClasses-Experiment'.
	exp := {'classVariableNames:' . '' . 'package:' . '''UndefinedClasses-Experiment'}.
	self assert: ((self scannerClass scanTokenObjects: inp) collect: [ :each | each value ]) equals: exp
]

{ #category : #utilities }
RBScannerTest >> verifyErrorToken: scannedToken message: message valueExpected: valueExpected [
	self assert: scannedToken isError.
	self assert: scannedToken cause equals: message.
	self assert: scannedToken value equals: valueExpected.
	self assert: scannedToken start equals: 1
]
