some tests
testUnion
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