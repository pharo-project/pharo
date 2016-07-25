*Random-Tests
testAtRandom
	| dict random |
	dict := { #one -> 1 . #two -> 2 . #three -> 3  } asOrderedDictionary.
	random := dict atRandom.
	self assert: (#(1 2 3) includes: random).
	self assert: (dict includes: random).
	