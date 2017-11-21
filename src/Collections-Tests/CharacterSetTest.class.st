"
CharacterSetTest holds tests for CharacterSet
"
Class {
	#name : #CharacterSetTest,
	#superclass : #TestCase,
	#category : #Collections-Tests-Support
}

{ #category : #tests }
CharacterSetTest >> testCopy [
    | theOriginal theCopy |
    theOriginal := CharacterSet newFrom: 'abc'.
    theCopy := theOriginal copy.
    theCopy remove: $a.
    ^self should: [theOriginal includes: $a] description: 'Changing the copy should not change the original'.
]
