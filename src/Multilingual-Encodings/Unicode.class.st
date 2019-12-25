"
This class holds the entry points for the utility functions around characters.

"
Class {
	#name : #Unicode,
	#superclass : #EncodedCharSet,
	#classVars : [
		'Cc',
		'Cf',
		'Cn',
		'Co',
		'Cs',
		'DecimalProperty',
		'GeneralCategory',
		'Ll',
		'Lm',
		'Lo',
		'Lt',
		'Lu',
		'Mc',
		'Me',
		'Mn',
		'Nd',
		'Nl',
		'No',
		'Pc',
		'Pd',
		'Pe',
		'Pf',
		'Pi',
		'Po',
		'Ps',
		'Sc',
		'Sk',
		'Sm',
		'So',
		'ToCasefold',
		'ToLower',
		'ToUpper',
		'Zl',
		'Zp',
		'Zs',
		'nonCharacters'
	],
	#category : #'Multilingual-Encodings-CharSets'
}

{ #category : #comments }
Unicode class >> blocks320Comment [

"# Blocks-3.2.0.txt
# Correlated with Unicode 3.2
# Start Code..End Code; Block Name
0000..007F; Basic Latin
0080..00FF; Latin-1 Supplement
0100..017F; Latin Extended-A
0180..024F; Latin Extended-B
0250..02AF; IPA Extensions
02B0..02FF; Spacing Modifier Letters
0300..036F; Combining Diacritical Marks
0370..03FF; Greek and Coptic
0400..04FF; Cyrillic
0500..052F; Cyrillic Supplementary
0530..058F; Armenian
0590..05FF; Hebrew
0600..06FF; Arabic
0700..074F; Syriac
0780..07BF; Thaana
0900..097F; Devanagari
0980..09FF; Bengali
0A00..0A7F; Gurmukhi
0A80..0AFF; Gujarati
0B00..0B7F; Oriya
0B80..0BFF; Tamil
0C00..0C7F; Telugu
0C80..0CFF; Kannada
0D00..0D7F; Malayalam
0D80..0DFF; Sinhala
0E00..0E7F; Thai
0E80..0EFF; Lao
0F00..0FFF; Tibetan
1000..109F; Myanmar
10A0..10FF; Georgian
1100..11FF; Hangul Jamo
1200..137F; Ethiopic
13A0..13FF; Cherokee
1400..167F; Unified Canadian Aboriginal Syllabics
1680..169F; Ogham
16A0..16FF; Runic
1700..171F; Tagalog
1720..173F; Hanunoo
1740..175F; Buhid
1760..177F; Tagbanwa
1780..17FF; Khmer
1800..18AF; Mongolian
1E00..1EFF; Latin Extended Additional
1F00..1FFF; Greek Extended
2000..206F; General Punctuation
2070..209F; Superscripts and Subscripts
20A0..20CF; Currency Symbols
20D0..20FF; Combining Diacritical Marks for Symbols
2100..214F; Letterlike Symbols
2150..218F; Number Forms
2190..21FF; Arrows
2200..22FF; Mathematical Operators
2300..23FF; Miscellaneous Technical
2400..243F; Control Pictures
2440..245F; Optical Character Recognition
2460..24FF; Enclosed Alphanumerics
2500..257F; Box Drawing
2580..259F; Block Elements
25A0..25FF; Geometric Shapes
2600..26FF; Miscellaneous Symbols
2700..27BF; Dingbats
27C0..27EF; Miscellaneous Mathematical Symbols-A
27F0..27FF; Supplemental Arrows-A
2800..28FF; Braille Patterns
2900..297F; Supplemental Arrows-B
2980..29FF; Miscellaneous Mathematical Symbols-B
2A00..2AFF; Supplemental Mathematical Operators
2E80..2EFF; CJK Radicals Supplement
2F00..2FDF; Kangxi Radicals
2FF0..2FFF; Ideographic Description Characters
3000..303F; CJK Symbols and Punctuation
3040..309F; Hiragana
30A0..30FF; Katakana
3100..312F; Bopomofo
3130..318F; Hangul Compatibility Jamo
3190..319F; Kanbun
31A0..31BF; Bopomofo Extended
31F0..31FF; Katakana Phonetic Extensions
3200..32FF; Enclosed CJK Letters and Months
3300..33FF; CJK Compatibility
3400..4DBF; CJK Unified Ideographs Extension A
4E00..9FFF; CJK Unified Ideographs
A000..A48F; Yi Syllables
A490..A4CF; Yi Radicals
AC00..D7AF; Hangul Syllables
D800..DB7F; High Surrogates
DB80..DBFF; High Private Use Surrogates
DC00..DFFF; Low Surrogates
E000..F8FF; Private Use Area
F900..FAFF; CJK Compatibility Ideographs
FB00..FB4F; Alphabetic Presentation Forms
FB50..FDFF; Arabic Presentation Forms-A
FE00..FE0F; Variation Selectors
FE20..FE2F; Combining Half Marks
FE30..FE4F; CJK Compatibility Forms
FE50..FE6F; Small Form Variants
FE70..FEFF; Arabic Presentation Forms-B
FF00..FFEF; Halfwidth and Fullwidth Forms
FFF0..FFFF; Specials
10300..1032F; Old Italic
10330..1034F; Gothic
10400..1044F; Deseret
1D000..1D0FF; Byzantine Musical Symbols
1D100..1D1FF; Musical Symbols
1D400..1D7FF; Mathematical Alphanumeric Symbols
20000..2A6DF; CJK Unified Ideographs Extension B
2F800..2FA1F; CJK Compatibility Ideographs Supplement
E0000..E007F; Tags
F0000..FFFFF; Supplementary Private Use Area-A
100000..10FFFF; Supplementary Private Use Area-B


"
]

{ #category : #comments }
Unicode class >> blocks320Comment2 [

"# Blocks-3.2.0.txt
# Correlated with Unicode 3.2
# Start Code..End Code; Block Name
0000..007F; Basic Latin
0080..00FF; Latin-1 Supplement

 => Latin 1

0100..017F; Latin Extended-A
0180..024F; Latin Extended-B
0250..02AF; IPA Extensions

  => LatinExtended1

02B0..02FF; Spacing Modifier Letters
0300..036F; Combining Diacritical Marks

  => Modifiers

0370..03FF; Greek and Coptic
0400..04FF; Cyrillic
0500..052F; Cyrillic Supplementary
0530..058F; Armenian

   => EuropeanAlphabetic1

0590..05FF; Hebrew
0600..06FF; Arabic
0700..074F; Syriac
0780..07BF; Thaana

   => MiddleEastern

0900..097F; Devanagari
0980..09FF; Bengali
0A00..0A7F; Gurmukhi
0A80..0AFF; Gujarati
0B00..0B7F; Oriya
0B80..0BFF; Tamil
0C00..0C7F; Telugu
0C80..0CFF; Kannada
0D00..0D7F; Malayalam
0D80..0DFF; Sinhala

  => South Asian1


0E00..0E7F; Thai
0E80..0EFF; Lao

 => Southeastern 1

0F00..0FFF; Tibetan

  => South Asian1

1000..109F; Myanmar

 => Southeastern 1


10A0..10FF; Georgian

   => European Alphabetic 2

1100..11FF; Hangul Jamo

   => Korean

1200..137F; Ethiopic
13A0..13FF; Cherokee
1400..167F; Unified Canadian Aboriginal Syllabics

  => Additional1

1680..169F; Ogham
16A0..16FF; Runic

  => European Alphabetic 3

1700..171F; Tagalog
1720..173F; Hanunoo
1740..175F; Buhid
1760..177F; Tagbanwa
1780..17FF; Khmer

  => Southeastern2

1800..18AF; Mongolian

  => Additional2

1E00..1EFF; Latin Extended Additional
1F00..1FFF; Greek Extended

  => EuropeanAlphabetic4

2000..206F; General Punctuation
2070..209F; Superscripts and Subscripts
20A0..20CF; Currency Symbols
20D0..20FF; Combining Diacritical Marks for Symbols
2100..214F; Letterlike Symbols
2150..218F; Number Forms
2190..21FF; Arrows
2200..22FF; Mathematical Operators
2300..23FF; Miscellaneous Technical
2400..243F; Control Pictures
2440..245F; Optical Character Recognition
2460..24FF; Enclosed Alphanumerics
2500..257F; Box Drawing
2580..259F; Block Elements
25A0..25FF; Geometric Shapes
2600..26FF; Miscellaneous Symbols
2700..27BF; Dingbats
27C0..27EF; Miscellaneous Mathematical Symbols-A
27F0..27FF; Supplemental Arrows-A
2800..28FF; Braille Patterns
2900..297F; Supplemental Arrows-B
2980..29FF; Miscellaneous Mathematical Symbols-B
2A00..2AFF; Supplemental Mathematical Operators

  => Symbols2

2E80..2EFF; CJK Radicals Supplement
2F00..2FDF; Kangxi Radicals
2FF0..2FFF; Ideographic Description Characters
3000..303F; CJK Symbols and Punctuation
3040..309F; Hiragana
30A0..30FF; Katakana
3100..312F; Bopomofo
3130..318F; Hangul Compatibility Jamo
3190..319F; Kanbun
31A0..31BF; Bopomofo Extended
31F0..31FF; Katakana Phonetic Extensions
3200..32FF; Enclosed CJK Letters and Months
3300..33FF; CJK Compatibility
3400..4DBF; CJK Unified Ideographs Extension A
4E00..9FFF; CJK Unified Ideographs
A000..A48F; Yi Syllables
A490..A4CF; Yi Radicals

  => CJK

AC00..D7AF; Hangul Syllables

  => Korean

D800..DB7F; High Surrogates
DB80..DBFF; High Private Use Surrogates
DC00..DFFF; Low Surrogates
E000..F8FF; Private Use Area

F900..FAFF; CJK Compatibility Ideographs

  => CJK

FB00..FB4F; Alphabetic Presentation Forms
FB50..FDFF; Arabic Presentation Forms-A

  => Middle Eastern 2

FE00..FE0F; Variation Selectors
FE20..FE2F; Combining Half Marks

FE30..FE4F; CJK Compatibility Forms

  => CJK

FE50..FE6F; Small Form Variants

 => Symbol3

FE70..FEFF; Arabic Presentation Forms-B

  => Middle Eastern 3

FF00..FFEF; Halfwidth and Fullwidth Forms
FFF0..FFFF; Specials

  => Specials

10300..1032F; Old Italic
10330..1034F; Gothic
10400..1044F; Deseret

   => European

1D000..1D0FF; Byzantine Musical Symbols
1D100..1D1FF; Musical Symbols
1D400..1D7FF; Mathematical Alphanumeric Symbols

  => Symbols

20000..2A6DF; CJK Unified Ideographs Extension B
2F800..2FA1F; CJK Compatibility Ideographs Supplement

  => CJK

E0000..E007F; Tags
F0000..FFFFF; Supplementary Private Use Area-A
100000..10FFFF; Supplementary Private Use Area-B

  => Special

"
]

{ #category : #'instance creation' }
Unicode class >> charFromUnicode: uniCode [

	^ Character value: uniCode

]

{ #category : #'class methods' }
Unicode class >> compoundTextFinalChar [

	self shouldNotImplement.

]

{ #category : #'class methods' }
Unicode class >> compoundTextSequence [

	self subclassResponsibility.

]

{ #category : #'class methods' }
Unicode class >> digitValueOf: char [
	"Answer 0-9 if the receiver is $0-$9, 10-35 if it is $A-$Z, and < 0
	otherwise. This is used to parse literal numbers of radix 2-36."
	
	| value |
	value := char charCode.
	value <= $9 asciiValue 
		ifTrue: [^value - $0 asciiValue].
	value >= $A asciiValue 
		ifTrue: [ value <= $Z asciiValue ifTrue: [^value - $A asciiValue + 10].
				(value >= $a asciiValue and: [value <= $z asciiValue]) 
					ifTrue: [^value - $a asciiValue + 10]].

	value > (DecimalProperty size - 1) ifTrue: [^ -1].
	^ (DecimalProperty at: value+1)

]

{ #category : #'class methods' }
Unicode class >> generalCategory [

	^ GeneralCategory.


]

{ #category : #'class methods' }
Unicode class >> generalCategoryComment [
"
Lu Letter, Uppercase 
Ll Letter, Lowercase 
Lt Letter, Titlecase 
Lm Letter, Modifier 
Lo Letter, Other 
Mn Mark, Non-Spacing 
Mc Mark, Spacing Combining 
Me Mark, Enclosing 
Nd Number, Decimal 
Nl Number, Letter 
No Number, Other 
Pc Punctuation, Connector 
Pd Punctuation, Dash 
Ps Punctuation, Open 
Pe Punctuation, Close 
Pi Punctuation, Initial quote (may behave like Ps or Pe depending on usage) 
Pf Punctuation, Final quote (may behave like Ps or Pe depending on usage) 
Po Punctuation, Other 
Sm Symbol, Math 
Sc Symbol, Currency 
Sk Symbol, Modifier 
So Symbol, Other 
Zs Separator, Space 
Zl Separator, Line 
Zp Separator, Paragraph 
Cc Other, Control 
Cf Other, Format 
Cs Other, Surrogate 
Co Other, Private Use 
Cn Other, Not Assigned (no characters in the file have this property) 
"
]

{ #category : #'class initialization' }
Unicode class >> initialize [
	"This class should be initialized using the textual unicode definitions that can be found in
	http://www.unicode.org/Public/UNIDATA/UnicodeData.txt
	http://www.unicode.org/Public/UNIDATA/CaseFolding.txt
	
	To do that, this class posesses the class side methods:
	#parseCaseMappingFrom:
	#parseUnicodeDataFrom:
	
	The initialization order is:
	1) tag constants
	2) unicode data
	3) case mappings
	"
	"If you are in a full image, you can try the following Zinc scripts to download the file automatically:
	
	UIManager default informUserDuring: [ :bar| | result |
            bar label: 'Downloading Unicode data'.
            (result := ZnEasy get: 'http://www.unicode.org/Public/UNIDATA/UnicodeData.txt') isSuccess
                ifFalse: [ ^ self error: 'Download failed' ].
            bar label: 'Updating Case Mappings'.
            self parseUnicodeDataFrom: result contents readStream ].

	UIManager default informUserDuring: [ :bar| | result |
		bar label: 'Downloading Unicode case mappings data'.
		(result := ZnEasy get: 'http://www.unicode.org/Public/UNIDATA/CaseFolding.txt') isSuccess
			ifFalse: [ ^ self error: 'Download failed' ].
		bar label: 'Updating Case Mappings'.
		self parseCaseMappingFrom: result contents readStream ].
	"
	
	EncodedCharSet declareEncodedCharSet: self atIndex: 0+1.
	EncodedCharSet declareEncodedCharSet: self atIndex: 256.

]

{ #category : #'class initialization' }
Unicode class >> initializeTagConstants [
	" Unicode initializeTagConstants "
	(self classPool keys
		select: [:sym | sym size = 2 
						and: [sym first isUppercase and: [sym last isLowercase]]]) asSortedCollection
		inject: 1
		into: [:index :sym | sym = #Cn
				ifTrue: [self classPool at: sym put: 0. index]
				ifFalse: [self classPool at: sym put: index. index + 1]]
]

{ #category : #private }
Unicode class >> is: char inCategory: cat [
	| index |
	index := char charCode + 1.
	index > GeneralCategory size
		ifTrue: [ ^ false ].
	^ (GeneralCategory at: index) = cat
]

{ #category : #'character classification' }
Unicode class >> isCasedLetter: char [
	"There are no Unicode Characters in this Category"
	^ false
]

{ #category : #'class methods' }
Unicode class >> isCharset [

	^ false.

]

{ #category : #'character classification' }
Unicode class >> isClosePunctuation: char [
	^ self is: char inCategory: Pe
]

{ #category : #'character classification' }
Unicode class >> isConnectorPunctuation: char [
	^ self is: char inCategory: Pc
]

{ #category : #'character classification' }
Unicode class >> isControlOther: char [ 
	^ self is: char inCategory: Cc
]

{ #category : #'character classification' }
Unicode class >> isCurrencySymbol: char [
	^ self is: char inCategory: Sc
]

{ #category : #'character classification' }
Unicode class >> isDashPunctuation: char [
	^ self is: char inCategory: Pd
]

{ #category : #'character classification' }
Unicode class >> isDecimalDigit: char [
	^ self is: char inCategory: Nd
]

{ #category : #'character classification' }
Unicode class >> isDigit: char [
	^ self is: char inCategory: Nd
]

{ #category : #'character classification' }
Unicode class >> isEnclosingMark: char [
	
	^ self is: char inCategory: Me
]

{ #category : #'character classification' }
Unicode class >> isFinalQuote: char [
	^ self is: char inCategory: Pf
]

{ #category : #'character classification' }
Unicode class >> isFormatOther: char [ 
	^ self is: char inCategory: Cf
]

{ #category : #'character classification' }
Unicode class >> isInitialQuote: char [
	^ self is: char inCategory: Pi
]

{ #category : #'character classification' }
Unicode class >> isLetter: char [
	| index |
	index := char charCode + 1.
	index > GeneralCategory size
		ifTrue: [ ^ false ].
	^ (GeneralCategory at: index) between: Ll and: Lu
]

{ #category : #'character classification' }
Unicode class >> isLetterModifier: char [ 
	| index |
	index := char charCode + 1.
	index > GeneralCategory size
		ifTrue: [^ false].
	^ (GeneralCategory at: index) = Lm
]

{ #category : #'character classification' }
Unicode class >> isLetterNumber: char [
	^ self is: char inCategory: Nl
]

{ #category : #'character classification' }
Unicode class >> isLineSeparator: char [
	^ self is: char inCategory: Zl
]

{ #category : #'character classification' }
Unicode class >> isLowercase: char [ 
	| index |
	index := char charCode + 1.
	index > GeneralCategory size
		ifTrue: [^ false].
	^ (GeneralCategory at: index) = Ll
]

{ #category : #'character classification' }
Unicode class >> isMathSymbol: char [
	^ self is: char inCategory: Sm
]

{ #category : #'character classification' }
Unicode class >> isModifierSymbol: char [
	^ self is: char inCategory: Sk
]

{ #category : #'character classification' }
Unicode class >> isNonCharacter: ch [
	^ self nonCharacters includes: ch codePoint 
]

{ #category : #'character classification' }
Unicode class >> isNonspacingMark: char [
	^ self is: char inCategory: Mn
]

{ #category : #'character classification' }
Unicode class >> isOpenPunctuation: char [
	^ self is: char inCategory: Ps
]

{ #category : #'character classification' }
Unicode class >> isOtherLetter: char [ 
	| index |
	index := char charCode + 1.
	index > GeneralCategory size
		ifTrue: [^ false].
	^ (GeneralCategory at: index) = Lo
]

{ #category : #'character classification' }
Unicode class >> isOtherNumber: char [
	^ self is: char inCategory: No
]

{ #category : #'character classification' }
Unicode class >> isOtherPunctuation: char [
	^ self is: char inCategory: Po
]

{ #category : #'character classification' }
Unicode class >> isOtherSymbol: char [
	^ self is: char inCategory: So
]

{ #category : #'character classification' }
Unicode class >> isParagraphSeparator: char [
	^ self is: char inCategory: Zp
]

{ #category : #'character classification' }
Unicode class >> isPrivateOther: char [
	^ self is: char inCategory: Co
]

{ #category : #'character classification' }
Unicode class >> isSpaceSeparator: char [
	^ self is: char inCategory: Zs
]

{ #category : #'character classification' }
Unicode class >> isSpacingCombiningMark: char [
	^ self is: char inCategory: Mc
]

{ #category : #'character classification' }
Unicode class >> isSurrogateOther: char [ 
	| index |
	index := char charCode + 1.
	index > (GeneralCategory size)
		ifTrue: [ ^ false ].
	^ (GeneralCategory at: index) = Cs
]

{ #category : #'character classification' }
Unicode class >> isTitlecaseLetter: char [ 
	| index |
	index := char charCode + 1.
	index > GeneralCategory size
		ifTrue: [^ false].
	^ (GeneralCategory at: index) = Lt
]

{ #category : #'character classification' }
Unicode class >> isUppercase: char [ 
	| index |
	index := char charCode + 1.
	index > GeneralCategory size
		ifTrue: [^ false].
	^ (GeneralCategory at: index) = Lu
]

{ #category : #'class methods' }
Unicode class >> leadingChar [
	^ 0
]

{ #category : #sizing }
Unicode class >> maxValue [
	"The maximum value of a character in this character set"

	^ 16r10FFFF
]

{ #category : #'class initialization' }
Unicode class >> nonCharacters [
	nonCharacters
		ifNil: [ nonCharacters := (16rFDD0 to: 16rFDEF) asOrderedCollection
				, #(16rFFFE 16rFFFF 16r1FFFE 16r1FFFF 16r0002FFFE 16r0002FFFF 16r0003FFFE 16r0003FFFF)
				, (16r4FFFE to: 16rFFFFE by: 16r10000) , (16r4FFFF to: 16rFFFFF by: 16r10000)
				, #(16r0010FFFE 16r0010FFFF) ].
	^ nonCharacters
]

{ #category : #'class initialization' }
Unicode class >> parseCaseMappingFrom: stream [
	"Parse the Unicode casing mappings from the given stream.
	Handle only the simple mappings"

	ToCasefold := IdentityDictionary new: 2048.
	ToUpper := IdentityDictionary new: 2048.
	ToLower := IdentityDictionary new: 2048.
	[ stream atEnd ]
		whileFalse: [ 
			| fields line srcCode dstCode |
			line := stream nextLine copyUpTo: $#.
			fields := line trimBoth findTokens: $;.
			(fields size > 2 and: [ #('C' 'S') includes: (fields at: 2) trimBoth ])
				ifTrue: [ 
					srcCode := Integer readFrom: (fields at: 1) trimBoth base: 16.
					dstCode := Integer readFrom: (fields at: 3) trimBoth base: 16.
					ToCasefold at: srcCode put: dstCode ] ].
	ToCasefold
		keysAndValuesDo: [ :k :v | 
			(self isUppercase: (self value: k))
				ifTrue: [ 
					"In most cases, uppercase letter are folded to lower case"
					ToUpper at: v ifAbsentPut: k.
					ToLower at: k ifAbsentPut: v ].
			(self isLowercase: (self value: k))
				ifTrue: [ 
					"In a few cases, two lower case letters are folded to the same lower case.
				We must find an upper case letter folded to the same letter"
					ToCasefold keys
						detect: [ :e | (self isUppercase: (self value: e)) and: [ (ToCasefold at: e) = v ] ]
						ifFound: [ :up | ToUpper at: k ifAbsentPut: up ] ] ]
]

{ #category : #'class initialization' }
Unicode class >> parseUnicodeDataFrom: stream [
	| line fieldEnd point fieldStart toNumber generalCategory decimalProperty loValue |

	toNumber := [:quad | ('16r', quad) asNumber].

	GeneralCategory := SparseLargeTable new: 16rE0080 chunkSize: 1024 arrayClass: Array base: 1 defaultValue:  (self classPool at: #Cn).
	DecimalProperty := SparseLargeTable new: 16rE0080 chunkSize: 32 arrayClass: Array base: 1 defaultValue: -1.

	loValue := (self classPool at: #Lo).
	16r3400 to: 16r4DB5 do: [:i | GeneralCategory at: i+1 put: loValue].
	16r4E00 to: 16r9FA5 do: [:i | GeneralCategory at: i+1 put: loValue].
	16rAC00 to: 16rD7FF do: [:i | GeneralCategory at: i+1 put: loValue].

	[(line := stream nextLine) notNil and: [line size > 0 ]] whileTrue: [
		fieldEnd := line indexOf: $; startingAt: 1.
		point := toNumber value: (line copyFrom: 1 to: fieldEnd - 1).
		point > 16rE007F ifTrue: [
			GeneralCategory zapDefaultOnlyEntries.
			DecimalProperty zapDefaultOnlyEntries.
			^ self].
		2 to: 3 do: [:i |
			fieldStart := fieldEnd + 1.
			fieldEnd := line indexOf: $; startingAt: fieldStart.
		].
		generalCategory := line copyFrom: fieldStart to: fieldEnd - 1.
		GeneralCategory at: point+1 put: (self classPool at: generalCategory asSymbol).
		generalCategory = 'Nd' ifTrue: [
			4 to: 7 do: [:i |
				fieldStart := fieldEnd + 1.
				fieldEnd := line indexOf: $; startingAt: fieldStart.
			].
			decimalProperty :=  line copyFrom: fieldStart to: fieldEnd - 1.
			DecimalProperty at: point+1 put: decimalProperty asNumber.
		].
	].
	GeneralCategory zapDefaultOnlyEntries.
	DecimalProperty zapDefaultOnlyEntries.

]

{ #category : #casing }
Unicode class >> toCasefold: aWideString [
	"Transform a Wide String into fold case.
	This is to enable case insensitive conversion."
	
	^aWideString collect: [:e |
		(ToCasefold at: e charCode ifAbsent: [nil])
			ifNil: [e]
			ifNotNil: [:low | self value: low]]
]

{ #category : #casing }
Unicode class >> toLowercase: aCharacter [
	"Transform a Wide String into lowercase.
	This does not handle special cases where number of characters could change.
	The algorithm would work for ByteString, however it's far from the most efficient."
	
	(ToLower at: aCharacter charCode ifAbsent: [ ^ aCharacter ])
			ifNotNil: [:low | ^ self value: low ]
]

{ #category : #casing }
Unicode class >> toLowercaseString: aWideString [
	"Transform a Wide String into lowercase.
	This does not handle special cases where number of characters could change.
	The algorithm would work for ByteString, however it's far from the most efficient."
	
	^aWideString collect: [:e |
		(ToLower at: e charCode ifAbsent: [nil])
			ifNil: [e]
			ifNotNil: [:low | self value: low]]
]

{ #category : #casing }
Unicode class >> toUppercase: aCharacter [
	"Transform a Wide String into uppercase.
	This does not handle special cases where number of characters could change.
	The algorithm would work for ByteString, however it's far from the most efficient."
	
	(ToUpper at: aCharacter charCode ifAbsent: [ ^ aCharacter ])
			ifNotNil: [:up | ^ self value: up]
]

{ #category : #casing }
Unicode class >> toUppercaseString: aWideString [
	"Transform a Wide String into uppercase.
	This does not handle special cases where number of characters could change.
	The algorithm would work for ByteString, however it's far from the most efficient."
	
	^aWideString collect: [:e |
		(ToUpper at: e charCode ifAbsent: [nil])
			ifNil: [e]
			ifNotNil: [:up | self value: up]]
]

{ #category : #'character classification' }
Unicode class >> ucsTable [
	^ Latin1 ucsTable 
]

{ #category : #'instance creation' }
Unicode class >> value: code [

	^ Character value: code
]
