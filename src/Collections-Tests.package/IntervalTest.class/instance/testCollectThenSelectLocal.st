tests
testCollectThenSelectLocal
	| letters vowels |
	letters := 'abcdefghijklmnopqrstuvwxyz'.
	vowels := (1 to: 26) collect: [:index | letters at: index] thenSelect: [:char | char isVowel].
	self assert: (vowels hasEqualElements: 'aeiou').