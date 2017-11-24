"
This class defines the attributes of the ASCII character set.  It's here to be used while boostrapping the image; eventually, it will be replaced by the
Unicode character set.

Character objects delegate behaviour to one of the  character sets AsciiCharSet, Latin1, or Unicode. 
"
Class {
	#name : #AsciiCharset,
	#superclass : #Object,
	#category : #'Kernel-BasicObjects'
}

{ #category : #testing }
AsciiCharset class >> canBeGlobalVarInitial: aCharacter [ 
	
	^ self isUppercase: aCharacter
]

{ #category : #'character classification' }
AsciiCharset class >> isCasedLetter: char [
	"There are no ASCII Characters in this Category"
	^ false
]

{ #category : #'character classification' }
AsciiCharset class >> isClosePunctuation: char [
	^ ')]}' includes: char
]

{ #category : #'character classification' }
AsciiCharset class >> isConnectorPunctuation: char [
	^ char = $_
]

{ #category : #'character classification' }
AsciiCharset class >> isControlOther: char [
	char charCode <= 16r1F ifTrue: [ ^ true ].
	^ char = Character delete 
]

{ #category : #'character classification' }
AsciiCharset class >> isCurrencySymbol: char [
	^ char = $$
]

{ #category : #'character classification' }
AsciiCharset class >> isDashPunctuation: char [
	^ char = $-
]

{ #category : #'character classification' }
AsciiCharset class >> isDecimalDigit: char [
	^ self isDigit: char
]

{ #category : #'character classification' }
AsciiCharset class >> isDigit: aCharacter [

	^ aCharacter between: $0 and: $9
]

{ #category : #'character classification' }
AsciiCharset class >> isEnclosingMark: char [
	^ false
]

{ #category : #'character classification' }
AsciiCharset class >> isFinalQuote: char [
	^ false
]

{ #category : #'character classification' }
AsciiCharset class >> isFormatOther: char [ 
	^ false
]

{ #category : #'character classification' }
AsciiCharset class >> isInitialQuote: char [
	^ false
]

{ #category : #'character classification' }
AsciiCharset class >> isLetter: aCharacter [

	^ (aCharacter between: $a and: $z)
		or: [ (aCharacter between: $A and: $Z) ]	
]

{ #category : #'character classification' }
AsciiCharset class >> isLetterModifier: char [ 
	^ false
]

{ #category : #'character classification' }
AsciiCharset class >> isLetterNumber: char [
	^ false
]

{ #category : #'character classification' }
AsciiCharset class >> isLineSeparator: char [
	^ false
]

{ #category : #'character classification' }
AsciiCharset class >> isLowercase: aCharacter [

	^ (aCharacter between: $a and: $z)
]

{ #category : #'character classification' }
AsciiCharset class >> isMathSymbol: char [
	^ '+<=>|~' includes: char
]

{ #category : #'character classification' }
AsciiCharset class >> isModifierSymbol: char [
	^ '^`' includes: char
]

{ #category : #'character classification' }
AsciiCharset class >> isNonspacingMark: char [
	^ false
]

{ #category : #'character classification' }
AsciiCharset class >> isOpenPunctuation: char [
	^ '([{' includes: char
]

{ #category : #'character classification' }
AsciiCharset class >> isOtherLetter: char [ 
	^ false
]

{ #category : #'character classification' }
AsciiCharset class >> isOtherNumber: char [
	^ false
]

{ #category : #'character classification' }
AsciiCharset class >> isOtherPunctuation: char [
	^ '!"#%&''*,./:;?@\' includes: char
]

{ #category : #'character classification' }
AsciiCharset class >> isOtherSymbol: char [
	^ false
]

{ #category : #'character classification' }
AsciiCharset class >> isParagraphSeparator: char [
	^ false
]

{ #category : #'character classification' }
AsciiCharset class >> isPrivateOther: char [
	^ false
]

{ #category : #'character classification' }
AsciiCharset class >> isSpaceSeparator: char [
	^ char = Character space
]

{ #category : #'character classification' }
AsciiCharset class >> isSpacingCombiningMark: char [
	^ false
]

{ #category : #'character classification' }
AsciiCharset class >> isSurrogateOther: char [ 
	^ false
]

{ #category : #'character classification' }
AsciiCharset class >> isTitlecaseLetter: char [ 
	^ false
]

{ #category : #'character classification' }
AsciiCharset class >> isUppercase: aCharacter [

	^ (aCharacter between: $A and: $Z)
]

{ #category : #sizing }
AsciiCharset class >> maxValue [
	"The maximum value of a character in this character set"

	^ 127
]

{ #category : #casing }
AsciiCharset class >> toLowercase: aCharacter [
	"(AsciiCharset toLowercase: $A) >>> $a.
	 (AsciiCharset  toLowercase: $a) >>> $a.
	 (AsciiCharset  toLowercase: $!) >>> $!"

	(aCharacter between: $A and: $Z)
		ifFalse: [ ^ aCharacter ].
	^ Character value: aCharacter asciiValue + $a asInteger - $A asInteger
]

{ #category : #casing }
AsciiCharset class >> toUppercase: aCharacter [
	"(AsciiCharset new toUppercase: $a) >>> $A.
	(AsciiCharset new toUppercase: $A) >>> $A.
	(AsciiCharset new toUppercase: $!) >>> $!"
	(aCharacter between: $a and: $z)
		ifFalse: [ ^ aCharacter ].
	
	^ Character value: (aCharacter asciiValue - $a asInteger + $A asInteger )
]

{ #category : #'character classification' }
AsciiCharset class >> ucsTable [
	^ Latin1 ucsTable 
]
