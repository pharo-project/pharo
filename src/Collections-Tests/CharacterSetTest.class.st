"
CharacterSetTest holds tests for CharacterSet
"
Class {
	#name : #CharacterSetTest,
	#superclass : #SetTest,
	#category : #'Collections-Tests-Support'
}

{ #category : #testing }
CharacterSetTest class >> shouldInheritSelectors [
	^ true
]

{ #category : #'tests - includes' }
CharacterSetTest >> anotherElementNotIn [
	^ Character codePoint: 254

	
]

{ #category : #coverage }
CharacterSetTest >> classToBeTested [
	^ CharacterSet 
]

{ #category : #requirements }
CharacterSetTest >> collectionWithSortableElements [
	^ nonEmpty1element 
]

{ #category : #requirements }
CharacterSetTest >> element [
	^ $x
]

{ #category : #'tests - includes' }
CharacterSetTest >> elementNotInForOccurrences [
	^ elementNotIn
]

{ #category : #'test - copy' }
CharacterSetTest >> elementToAdd [ 
	^ $p
]

{ #category : #running }
CharacterSetTest >> setUp [
	empty := self classToBeTested  new.
	full := self classToBeTested  with: $x with: $y with: $z with: $w.
	collectionIncluded := self classToBeTested  with: $y with: $x .
	collectionWith3Elements := self classToBeTested  new.
	collectionWith3Elements add: $a; add: $b; add: $c. 
	collectResult := self classToBeTested new.
	emptyButAllocatedWith20 := self classToBeTested new.
	elementNotIn := $q.
	collectionOfFloat := nil.
	nonEmpty1element := self classToBeTested  with: $ .
	withoutEqualElements := self classToBeTested  with: $r with: $s with: $w.
	collection5Elements := self classToBeTested with: $T with: $U with: $Y with: $W with: $X.
]

{ #category : #'tests - fixture' }
CharacterSetTest >> test0FixtureAddForUniquenessTest [
]

{ #category : #'tests - fixture' }
CharacterSetTest >> test0FixtureIncludeWithIdentityTest [
	"This test does not apply, because equal characters are also identical"
]

{ #category : #'tests - with nil' }
CharacterSetTest >> testAllowInclusionOfNils [
	"This test is not approriate for the class under test"
]

{ #category : #'test - copy' }
CharacterSetTest >> testAsByteArray [
	^ self
]

{ #category : #'tests - as string comma delimiter sequenceable' }
CharacterSetTest >> testAsStringOnDelimiterLastMore [
	"This test is not approriate for the class under test, since a CharacterSet cannot store strings."
]

{ #category : #'tests - as string comma delimiter sequenceable' }
CharacterSetTest >> testAsStringOnDelimiterMore [
	"This test is not approriate for the class under test, since a CharacterSet cannot store strings."
]

{ #category : #'some tests' }
CharacterSetTest >> testCollect2 [
	| newFull result |
	newFull := self classToBeTested withAll: ($A to: $J).
	result := newFull
		collect: [ :each | 
			each >= $A
				ifTrue: [ each ]
				ifFalse: [ $X ] ].
	self assert: result equals: newFull.
	result := newFull
		collect: [ :each | 
			each >= $E
				ifTrue: [ each ]
				ifFalse: [ $X ] ].
	self assert: result equals: ((Set withAll: ($E to: $J)) add: $X; yourself)
]

{ #category : #tests }
CharacterSetTest >> testCopy [
	| theOriginal theCopy |
	theOriginal := self classToBeTested newFrom: 'abc'.
	theCopy := theOriginal copy.
	self assert: theCopy equals: theOriginal.
	theCopy remove: $a.
	self deny: (theCopy includes: $a).
	self
		assert: (theOriginal includes: $a)
		description: 'Changing the copy should not change the original'
]

{ #category : #'tests - iterating' }
CharacterSetTest >> testDo2 [
	| setWithAtoE result |
	setWithAtoE := self classToBeTested withAll: ($A to: $E).
	result := Set new.
	setWithAtoE do: [ :each | result add: each].
	self assert: result size equals: setWithAtoE size.
	result do: [ :each | self assert: (setWithAtoE includes: each) ]
]

{ #category : #'test - iterate' }
CharacterSetTest >> testDoWithoutNoDuplicates [
	"self debug: #testDoWithoutNoDuplicates"

	| res |
	res := self speciesClass new.
	self collection do: [ :each | res add: each ] without: $a.
	self assert: res size equals: self doWithoutNumber
]

{ #category : #'sunit original tests' }
CharacterSetTest >> testGrow [
	"This test is not approriate for the class under test, since a CharacterSet does not grow."

	
]

{ #category : #'tests - includes' }
CharacterSetTest >> testIdentityIncludes [
]

{ #category : #tests }
CharacterSetTest >> testIntersection [ 
	| newFull col |
	full add: $y; add: $d.
	col := full intersection: full.
	self assert: full equals: col.

	newFull := self classToBeTested with: $A with: $B with: $C.
	col := newFull intersection: full.
	self assert: (col isEmpty).
	
	newFull add: $x; add: $y; add: $z.
	col := newFull intersection: full.
	self assert: (full select: [:each | (newFull includes: each)]) equals: col
	
]

{ #category : #'some tests' }
CharacterSetTest >> testIsHealthy [
	
]

{ #category : #'sunit original tests' }
CharacterSetTest >> testOccurrences [
	self assert: (empty occurrencesOf: $w) equals: 0.
	self assert: (full occurrencesOf: $w) equals: 1.
	full add: $w.
	self assert: (full occurrencesOf: $w) equals: 1
]

{ #category : #'sunit original tests' }
CharacterSetTest >> testRemove [
	| e |
	e := self element.
	full remove: e.
	self assert: (full includes: $w).
	self deny: (full includes: e)
]

{ #category : #'some tests' }
CharacterSetTest >> testUnion [
	| upper vowels result result2 |
	upper := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' as: CharacterSet.
	self assert: (upper isKindOf: CharacterSet).
	
	vowels := 'AEIOUaeiou' as: CharacterSet.
	self assert: (vowels isKindOf: CharacterSet).
	
	result := upper intersection: vowels.
	self assert: (result isKindOf: CharacterSet).
	self assert: result equals: 'AEIOU' asSet.
	
	result2 := vowels intersection: upper.
	self assert: (result2 isKindOf: CharacterSet).
	self assert: result2 equals: 'AEIOU' asSet
]

{ #category : #'some tests' }
CharacterSetTest >> testUnionEmpty [
	| upper result result2 lower |
	upper := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' as: CharacterSet.
	self assert: (upper isKindOf: CharacterSet).
	
	lower := 'aeiou' as: CharacterSet.
	self assert: (lower isKindOf: CharacterSet).
	
	result := lower intersection: upper.
	self assert: (result isKindOf: CharacterSet).
	self assert: result isEmpty.
	
	result2 := upper intersection: lower.
	self assert: (result2 isKindOf: CharacterSet).
	self assert: result2 isEmpty
]
