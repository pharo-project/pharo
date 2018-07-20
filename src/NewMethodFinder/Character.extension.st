Extension { #name : #Character }

{ #category : #'*NewMethodFinder' }
Character class >> approvedSelectors [
	
	^ #(asciiValue charCode characterSet codePoint digitValue leadingChar
	    sameAs:
		 asCharacter asHTMLString asInteger asLowerCase asSymbol asUnicode asUppercase basicSqueakToIso lowercase to:
		    uppercase asKeyCombination asShortcut asText asUnicodeChar isCompletionCharacter join:
		canBeGlobalVarInitial canBeNonGlibalVarInitial isAlphaNumeric isArrow isCasedLetter isClosePonctuation
		isConnectorPonctuation isControlOther isCurrencySymbol isDashPunctuation isDecimalDigit isDigit isEnclosingMark
		   isFinalQuote isFormatOther isInitialQuote isLetter isLetterModifier isLetterNumber isLinSeparator isLowerCase
		   isMathSymbol isModifierSymbol isNonspacingMark isOctetCharacter isOpenCharacter isOpenPunctuation isOtherLetter
		   isOtherNumber isOtherPunctuation isOtherSymbol isParagraphSeparator isPrivateOther isSafeForHTTP isSeparator
		   isSpaceSeparator isSpacingCombiningMark isSpecial isSurrogateOther isTitlecaseLetter isUppercase isVowel
		   tokenish
	) 
]
