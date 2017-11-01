"
An abstract superclasss of the classes that represent encoded character sets.  In the old implementation, the charsets had more important role.  However, in the current implementation, the subclasses are used only for keeping the backward compatibility.

	The other confusion comes from the name of ""Latin1"" class.  It used to mean the Latin-1 (ISO-8859-1) character set, but now it primarily means that the ""Western European languages that are covered by the characters in Latin-1 character set.

"
Class {
	#name : #EncodedCharSet,
	#superclass : #Object,
	#classVars : [
		'EncodedCharSets'
	],
	#classInstVars : [
		'compoundTextSequence',
		'ucsTable'
	],
	#category : 'Multilingual-Encodings'
}

{ #category : #'character classification' }
EncodedCharSet class >> canBeGlobalVarInitial: char [

	| leadingChar |
	leadingChar := char leadingChar.

	leadingChar = 0 ifTrue: [^ self isUppercase: char].
	^ self isLetter: char.

]

{ #category : #'character classification' }
EncodedCharSet class >> canBeNonGlobalVarInitial: char [

	| leadingChar |
	leadingChar := char leadingChar.

	leadingChar = 0 ifTrue: [^ self isLowercase: char].
	^ self isLetter: char.

]

{ #category : #'class methods' }
EncodedCharSet class >> charFromUnicode: unicode [

	| table index |
	unicode < 128 ifTrue: [^ Character value: unicode].

	table := self ucsTable.
	index := table indexOf: unicode.
	index = 0 ifTrue: [
		^ nil.
	].

	^ Character leadingChar: self leadingChar code: index - 1.


]

{ #category : #'class methods' }
EncodedCharSet class >> charsetAt: encoding [

	^ EncodedCharSets at: encoding + 1 ifAbsent: [Unicode].

]

{ #category : #initialize }
EncodedCharSet class >> declareEncodedCharSet: anEncodedCharSetOrLanguageEnvironmentClass atIndex: aNumber [
	
	EncodedCharSets at: aNumber put: anEncodedCharSetOrLanguageEnvironmentClass
	
	"this method is used to modularize the old initialize method: 
	EncodedCharSets at: 0+1 put: Unicode.
	EncodedCharSets at: 1+1 put: JISX0208.
	EncodedCharSets at: 2+1 put: GB2312.
	EncodedCharSets at: 3+1 put: KSX1001.
	EncodedCharSets at: 4+1 put: JISX0208.
	EncodedCharSets at: 5+1 put: JapaneseEnvironment.
	EncodedCharSets at: 6+1 put: SimplifiedChineseEnvironment.
	EncodedCharSets at: 7+1 put: KoreanEnvironment.
	EncodedCharSets at: 8+1 put: GB2312.
	EncodedCharSets at: 12+1 put: KSX1001.
	EncodedCharSets at: 13+1 put: GreekEnvironment.
	EncodedCharSets at: 14+1 put: Latin2Environment.
	EncodedCharSets at: 15+1 put: RussianEnvironment.
	EncodedCharSets at: 17+1 put: Latin9Environment.
	EncodedCharSets at: 256 put: Unicode.

	"
	
	
]

{ #category : #'class methods' }
EncodedCharSet class >> digitValueOf: char [
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
	^ -1

]

{ #category : #initialize }
EncodedCharSet class >> initialize [
	"self initialize"
	
	EncodedCharSets := Array new: 256.
	self allSubclassesDo: [:each | each initialize].
	LanguageEnvironment allSubclassesDo:  [:each | each initialize].


	"EncodedCharSets at: 0+1 put: Unicode.
	EncodedCharSets at: 1+1 put: JISX0208.
	EncodedCharSets at: 2+1 put: GB2312.
	EncodedCharSets at: 3+1 put: KSX1001.
	EncodedCharSets at: 4+1 put: JISX0208.
	EncodedCharSets at: 5+1 put: JapaneseEnvironment.
	EncodedCharSets at: 6+1 put: SimplifiedChineseEnvironment.
	EncodedCharSets at: 7+1 put: KoreanEnvironment.
	EncodedCharSets at: 8+1 put: GB2312.
	EncodedCharSets at: 12+1 put: KSX1001.
	EncodedCharSets at: 13+1 put: GreekEnvironment.
	EncodedCharSets at: 14+1 put: Latin2Environment.
	EncodedCharSets at: 15+1 put: RussianEnvironment.
	EncodedCharSets at: 17+1 put: Latin9Environment.
	EncodedCharSets at: 256 put: Unicode."

]

{ #category : #'accessing - displaying' }
EncodedCharSet class >> isBreakableAt: index in: text [

	self subclassResponsibility.

]

{ #category : #'class methods' }
EncodedCharSet class >> isCharset [

	^ true.

]

{ #category : #'character classification' }
EncodedCharSet class >> isDigit: char [
	"Answer whether the receiver is a digit."

	| value |
	value := char asciiValue.
	^ value >= 48 and: [value <= 57].

]

{ #category : #'character classification' }
EncodedCharSet class >> isLetter: char [
	"Answer whether the receiver is a letter."

	| value |
	value := char asciiValue.
	^ (8r141 <= value and: [value <= 8r172]) or: [8r101 <= value and: [value <= 8r132]].

]

{ #category : #'character classification' }
EncodedCharSet class >> isLowercase: char [
	"Answer whether the receiver is a lowercase letter.
	(The old implementation answered whether the receiver is not an uppercase letter.)"

	| value |
	value := char asciiValue.
	^ 8r141 <= value and: [value <= 8r172].

]

{ #category : #'character classification' }
EncodedCharSet class >> isUppercase: char [
	"Answer whether the receiver is an uppercase letter.
	(The old implementation answered whether the receiver is not a lowercase letter.)"

	| value |
	value := char asciiValue.
	^ 8r101 <= value and: [value <= 8r132].

]

{ #category : #'class methods' }
EncodedCharSet class >> leadingChar [

	self subclassResponsibility.

]

{ #category : #'class methods' }
EncodedCharSet class >> nextPutValue: ascii toStream: aStream withShiftSequenceIfNeededForTextConverterState: state [

	self subclassResponsibility.

]

{ #category : #'accessing - displaying' }
EncodedCharSet class >> printingDirection [

	self subclassResponsibility.

]

{ #category : #'accessing - displaying' }
EncodedCharSet class >> scanSelector [
	^ #scanMultiCharactersFrom:to:in:rightX:stopConditions:kern:
]

{ #category : #'class methods' }
EncodedCharSet class >> ucsTable [

	^ ucsTable 

]
