"
Tests the character classification methods against the generalCategory table.  

Note that the correctness of the table is not checked. 
It probebably should be, but that is another task. 
"
Class {
	#name : #UnicodeTest,
	#superclass : #TestCase,
	#instVars : [
		'unicodeGenerator'
	],
	#category : #'Kernel-Tests-Extended-Charset'
}

{ #category : #accessing }
UnicodeTest class >> resources [
	^ { UnicodeTestRNG }
]

{ #category : #utilities }
UnicodeTest class >> unicodeCategoryTableLookup: codePoint [
	| index table |
	index := codePoint + 1.
	table := Unicode generalCategory.
	^ index > table size
		ifTrue: [ -1 ]
		ifFalse: [ table at: index ]
]

{ #category : #utilities }
UnicodeTest >> aRandomSelectionOfCharactersDo: aBlock [
	self aRandomSelectionOfCodePointsDo: [ :cp | aBlock value: (Character codePoint: cp) ]
]

{ #category : #utilities }
UnicodeTest >> aRandomSelectionOfCodePointsDo: aBlock [
	0 to: 255 do: [ :cp | aBlock value: cp].
	500 timesRepeat: [ aBlock value: (unicodeGenerator randomCodePointAtOrAbove: 256) ].

]

{ #category : #utilities }
UnicodeTest >> checkCorrespondanceOf: aSelector and: aCategoryTag [
	| cat methodAnswer catFromTable |
	cat := Unicode classPool at: aCategoryTag.
	self
		aRandomSelectionOfCodePointsDo: [ :cp | 
			methodAnswer := Unicode
				perform: aSelector
				with: (Character codePoint: cp).
			catFromTable := self unicodeCategoryTableLookup: cp.
			methodAnswer = (catFromTable = cat)
				ifFalse: [ self
						assert: methodAnswer = (catFromTable = cat)
						description:
							aSelector asString , 'and category ' , aCategoryTag asString
								, 'disagree at U+' , cp asHexString ] ]
]

{ #category : #running }
UnicodeTest >> setUp [
	super setUp.
	unicodeGenerator := UnicodeTestRNG current
]

{ #category : #'tests - categorization' }
UnicodeTest >> testIsCasedLetter [
	"The Cased letter category, LC, is empty"
	self aRandomSelectionOfCharactersDo: [ :ch | 
		self deny: (Unicode isCasedLetter: ch) ]
]

{ #category : #'tests - categorization' }
UnicodeTest >> testIsClosePunctuation [
	self checkCorrespondanceOf: #isClosePunctuation: and: #Pe
]

{ #category : #'tests - categorization' }
UnicodeTest >> testIsConnectorPunctuation [
	self checkCorrespondanceOf: #isConnectorPunctuation: and: #Pc
]

{ #category : #'tests - categorization' }
UnicodeTest >> testIsControlOther [
	self checkCorrespondanceOf: #isControlOther: and: #Cc
]

{ #category : #'tests - categorization' }
UnicodeTest >> testIsCurrencySymbol [
	self checkCorrespondanceOf: #isCurrencySymbol: and: #Sc
]

{ #category : #'tests - categorization' }
UnicodeTest >> testIsDashPunctuation [
	self checkCorrespondanceOf: #isDashPunctuation: and: #Pd
]

{ #category : #'tests - categorization' }
UnicodeTest >> testIsDecimalDigit [
	self checkCorrespondanceOf: #isDecimalDigit: and: #Nd
]

{ #category : #'tests - categorization' }
UnicodeTest >> testIsDigit [
	self checkCorrespondanceOf: #isDigit: and: #Nd
]

{ #category : #'tests - categorization' }
UnicodeTest >> testIsEnclosingMark [
	self checkCorrespondanceOf: #isEnclosingMark: and: #Me
]

{ #category : #'tests - categorization' }
UnicodeTest >> testIsFinalQuote [
	self checkCorrespondanceOf: #isFinalQuote: and: #Pf
]

{ #category : #'tests - categorization' }
UnicodeTest >> testIsFormatOther [
	self checkCorrespondanceOf: #isFormatOther: and: #Cf
]

{ #category : #'tests - categorization' }
UnicodeTest >> testIsInitialQuote [
	self checkCorrespondanceOf: #isInitialQuote: and: #Pi
]

{ #category : #'tests - categorization' }
UnicodeTest >> testIsLetter [
	| letterCategories methodAnswer catFromTable |
	letterCategories := #(Ll Lm Lo Lt Lu) collect: [ :c | Unicode classPool at: c ].
	self aRandomSelectionOfCodePointsDo: [ :cp | 
			methodAnswer := Unicode isLetter: (Character codePoint: cp).
			catFromTable := self unicodeCategoryTableLookup: cp.
			self assert: methodAnswer equals: (letterCategories includes: catFromTable) ]
]

{ #category : #'tests - categorization' }
UnicodeTest >> testIsLetterModifier [
	self checkCorrespondanceOf: #isLetterModifier: and: #Lm
]

{ #category : #'tests - categorization' }
UnicodeTest >> testIsLetterNumber [
	self checkCorrespondanceOf: #isLetterNumber: and: #Nl
]

{ #category : #'tests - categorization' }
UnicodeTest >> testIsLineSeparator [
	self checkCorrespondanceOf: #isLineSeparator: and: #Zl
]

{ #category : #'tests - categorization' }
UnicodeTest >> testIsLowercase [
	self checkCorrespondanceOf: #isLowercase: and: #Ll
]

{ #category : #'tests - categorization' }
UnicodeTest >> testIsMathSymbol [
	self checkCorrespondanceOf: #isMathSymbol: and: #Sm
]

{ #category : #'tests - categorization' }
UnicodeTest >> testIsModifierSymbol [
	self checkCorrespondanceOf: #isModifierSymbol: and: #Sk
]

{ #category : #'tests - categorization' }
UnicodeTest >> testIsNonspacingMark [
	self checkCorrespondanceOf: #isNonspacingMark: and: #Mn
]

{ #category : #'tests - categorization' }
UnicodeTest >> testIsOpenPunctuation [
	self checkCorrespondanceOf: #isOpenPunctuation: and: #Ps
]

{ #category : #'tests - categorization' }
UnicodeTest >> testIsOtherLetter [
	self checkCorrespondanceOf: #isOtherLetter: and: #Lo
]

{ #category : #'tests - categorization' }
UnicodeTest >> testIsOtherNumber [
	self checkCorrespondanceOf: #isOtherNumber: and: #No
]

{ #category : #'tests - categorization' }
UnicodeTest >> testIsOtherPunctuation [
	self checkCorrespondanceOf: #isOtherPunctuation: and: #Po
]

{ #category : #'tests - categorization' }
UnicodeTest >> testIsOtherSymbol [
	self checkCorrespondanceOf: #isOtherSymbol: and: #So
]

{ #category : #'tests - categorization' }
UnicodeTest >> testIsParagraphSeparator [
	self checkCorrespondanceOf: #isParagraphSeparator: and: #Zp
]

{ #category : #'tests - categorization' }
UnicodeTest >> testIsPrivateOther [
	self checkCorrespondanceOf: #isPrivateOther: and: #Co
]

{ #category : #'tests - categorization' }
UnicodeTest >> testIsSpaceSeparator [
	self checkCorrespondanceOf: #isSpaceSeparator: and: #Zs
]

{ #category : #'tests - categorization' }
UnicodeTest >> testIsSpacingCombiningMark [
	self checkCorrespondanceOf: #isSpacingCombiningMark: and: #Mc
]

{ #category : #'tests - categorization' }
UnicodeTest >> testIsSurrogateOther [
	self checkCorrespondanceOf: #isSurrogateOther: and: #Cs
]

{ #category : #'tests - categorization' }
UnicodeTest >> testIsTitlecaseLetter [
	self checkCorrespondanceOf: #isTitlecaseLetter: and: #Lt
]

{ #category : #'tests - categorization' }
UnicodeTest >> testIsUppercase [
	self checkCorrespondanceOf: #isUppercase: and: #Lu
]

{ #category : #'tests - categorization' }
UnicodeTest >> testNonCharacterNegative [
	self aRandomSelectionOfCharactersDo: [  :ch |
		self deny: (Unicode isNonCharacter: ch) 
	]
]

{ #category : #'tests - categorization' }
UnicodeTest >> testNonCharacterPositive [
	| nonCps |
	nonCps := (16rFDD0 to: 16rFDEF) asSet. 
	nonCps addAll: (16r0FFFE to: 16r10FFFE by: 16r10000).
	nonCps addAll: (16r0FFFF to: 16r10FFFF by: 16r10000).
	self assert: nonCps size equals: 66.		"defined that way by the standard"
	nonCps do: [ :each | self assert: (Unicode isNonCharacter: (Character codePoint: each)) ]
]

{ #category : #tests }
UnicodeTest >> testRNG [
	| chars uniqueCount |
	chars := (1 to: 100)
		collect: [ :_ | unicodeGenerator randomCharacter].
	uniqueCount := chars asSet size.
	self assert: uniqueCount = 100 description: (100 - uniqueCount) asString , ' duplicates'
]

{ #category : #utilities }
UnicodeTest >> unicodeCategoryTableLookup: codePoint [

	^self class unicodeCategoryTableLookup: codePoint
]
