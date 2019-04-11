"
This is the unit test for the class Character. Unit tests are a good way to exercise the functionality of your system in a repeatable and automatic manner. They are therefore recommended if you plan to release anything. For more information, see: 
	- http://www.c2.com/cgi/wiki?UnitTest
	- there is a chapter in the PharoByExample book (http://pharobyexample.org/)
	- the sunit class category
"
Class {
	#name : #CharacterTest,
	#superclass : #ClassTestCase,
	#category : #'Collections-Tests-Strings'
}

{ #category : #coverage }
CharacterTest >> classToBeTested [
	
	^ Character
]

{ #category : #'tests - conversion' }
CharacterTest >> testAsLowercaseBasicAsciiRange [
	| lowercase uppercase |
	lowercase  := 'abcdefghijklmnopqrstuvwxyz123456789'.
	uppercase := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ123456789'.
	uppercase withIndexDo: [ :char :index |
		self assert: char asLowercase equals: (lowercase at: index)]
]

{ #category : #'tests - conversion' }
CharacterTest >> testAsUppercaseBasicAsciiRange [
	| lowercase uppercase |
	lowercase  := 'abcdefghijklmnopqrstuvwxyz123456789'.
	uppercase := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ123456789'.
	lowercase withIndexDo: [ :char :index |
		self assert: char asUppercase equals: (uppercase at: index)]
]

{ #category : #'tests - various' }
CharacterTest >> testCharacterSeparators [
	"Regression test"

	| result |
	result := '/' , Character separators.
	self assert: result size equals: Character separators size + 1 
]

{ #category : #tests }
CharacterTest >> testCodePoint [

	self assert: $A codePoint equals: 65.
	self assert: (Character codePoint: $a codePoint) equals: $a.
	self assert: (Character codePoint: 97) codePoint equals: 97.
]

{ #category : #tests }
CharacterTest >> testDigitValue [
		
	'0123456789ABCDEF' with: #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
		do: [:char :value | self assert: char digitValue equals: value].
	
	'0123456789' with: #(0 1 2 3 4 5 6 7 8 9)
		do: [:char :value | self assert: char digitValue equals: value].
		
	 
]

{ #category : #tests }
CharacterTest >> testHex [
	self assert: $a hex equals: '16r61'.
	self assert: Character space hex equals: '16r20'
]

{ #category : #'tests - instance creation' }
CharacterTest >> testInstanceCreation [
	self should: [ Character value: -1 ] raise: Error.
	Character value: 0.
	Character value: 256
]

{ #category : #tests }
CharacterTest >> testIsSeparator [
    {Character space.
    Character cr.
    Character lf.
    Character newPage.
    Character tab} do: [ :each | self assert: each isSeparator ].
    Character alphabet do: [ :each | self deny: each isSeparator ]
]

{ #category : #tests }
CharacterTest >> testMaxVal [
	"Check that Character class>>maxVal is returning the correct value:
	- A character created with the value has the same code point
	- Attempting to create a character with maxVal+1 fails"

	self assert: (Character value: Character maxVal) codePoint 
			equals: Character maxVal.
	self should: [ Character value: Character maxVal + 1 ]
			raise: PrimitiveFailed
]

{ #category : #'tests - instance creation' }
CharacterTest >> testNew [

	self should: [Character new] raise: Error.
]

{ #category : #tests }
CharacterTest >> testPrintString [
	self assert: $a printString equals: '$a'.
	self assert: $5 printString equals: '$5'.
	self assert: $@ printString equals: '$@'.

	self assert: Character cr printString equals: 'Character cr'.
	self assert: Character lf printString equals: 'Character lf'.
	self assert: Character space printString equals: 'Character space'.

	self assert: (Character value: 0) printString equals: 'Character null'.
	self assert: (Character value: 17) printString equals: 'Character value: 17'.
]

{ #category : #tests }
CharacterTest >> testPrintStringAll [
	Character allCharacters
		do: [ :each | self assert: (self class compiler evaluate: each printString) equals: each ]
]

{ #category : #tests }
CharacterTest >> testPrintStringHex [
	self assert: $a printStringHex equals: '61'.
	self assert: Character space printStringHex equals: '20'
]

{ #category : #tests }
CharacterTest >> testStoreString [
	self assert: $a storeString equals: '$a'.
	self assert: $5 storeString equals: '$5'.
	self assert: $@ storeString equals: '$@'.

	self assert: Character cr storeString equals: 'Character cr'.
	self assert: Character lf storeString equals: 'Character lf'.
	self assert: Character space storeString equals: 'Character space'.
	self assert: (Character value: 0) storeString equals: 'Character null'.
	
	self assert: (Character value: 17) storeString equals: '(Character value: 17)'.
]

{ #category : #tests }
CharacterTest >> testStoreStringAll [
	Character allCharacters
		do: [ :each | self assert: (self class compiler evaluate: each storeString) equals: each ]
]
