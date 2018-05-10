"
WideCharacterSetTest holds tests for WideCharacterSet
"
Class {
	#name : #WideCharacterSetTest,
	#superclass : #TestCase,
	#category : #'Collections-Tests-Support'
}

{ #category : #tests }
WideCharacterSetTest >> testAddingToCharacterSet [

	| cs wcs wc |
	cs := CharacterSet newFrom: 'aeiouy'.
	wcs := cs copy.
	wc := 4452 asCharacter.
	
	"adding a WideCharacter to an ordinary CharacterSet should turn it into a WideCharacterSet"
	wcs add: wc.

	self should: [wcs size = (cs size + 1)] description: 'We just added a Character, size should be increased by one'.
	self shouldnt: [wcs = cs] description: 'We just added a Character, sets should not be equal'.
	self shouldnt: [cs = wcs] description: 'We just added a Character, sets should not be equal'.
	self should: [cs allSatisfy: [:char | wcs includes: char]] description: 'Each character of the original CharacterSet should be included in the WideCharacterSet'.
	self should: [wcs hasWideCharacters] description: 'We just added a WideCharacter, so this WideCharacterSet definitely has one'.
	self should: [wcs includes: wc] description: 'We just added this WideCharacter, so this WideCharacterSet should include it'.
	
	wcs add: wc.
	self should: [wcs size = (cs size + 1)] description: 'We just added a Character already included in the set, size should be unchanged'.
	
	wcs remove: wc.
	self should: [wcs size = cs size] description: 'We added then removed a Character, now size should be equal to original'.
	self shouldnt: [wcs hasWideCharacters] description: 'We just removed the only WideCharacter, so this WideCharacterSet definitely has no WideCharacter'.
	
	self should: [wcs = cs] description: 'A WideCharacterSet can be equal to an Ordinary CharacterSet'.
	self should: [cs = wcs] description: 'An ordinary CharacterSet can be equal to a WideCharacterSet'.
	self should: [cs hash = wcs hash] description: 'If some objects are equal, then they should have same hash code'.
	
	
]

{ #category : #tests }
WideCharacterSetTest >> testCopy [
    | theOriginal theCopy |
    theOriginal := WideCharacterSet newFrom: ('abc' copyWith: 300 asCharacter).
    theCopy := theOriginal copy.
    theCopy remove: $a.
    ^self should: [theOriginal includes: $a] description: 'Changing the copy should not change the original'.
]

{ #category : #tests }
WideCharacterSetTest >> testCreation [
	"By now, only creation method is newFrom:"

	| cs1 wcs1 cs2 wcs2 byteString wideString |
	byteString := 'aeiouy'.
	wideString := 'aeiouy' copyWith: 340 asCharacter.

	cs1 := CharacterSet newFrom: byteString.
	wcs1 := WideCharacterSet newFrom: byteString.
	self assert: wcs1 equals: cs1.
	self assert: wcs1 size equals: byteString size.
	
	cs2 := CharacterSet newFrom: wideString.
	wcs2 := WideCharacterSet newFrom: wideString.
	self assert: wcs2 equals: cs2.
	self assert: wcs2 size equals: wideString size.
	
	self should: [(byteString indexOfAnyOf: wcs1) = 1] description: 'This should used optimized byteArrayMap method'.
	self should: [(byteString indexOfAnyOf: wcs2) = 1] description: 'This should used optimized byteArrayMap method'.
	
	self should: [('bcd' indexOfAnyOf: wcs1) = 0] description: 'This should used optimized byteArrayMap method'.
	self should: [('bcd' indexOfAnyOf: wcs2) = 0] description: 'This should used optimized byteArrayMap method'.
]
