utilities
substring: testString matches: aString caseSensitive: aBoolean
	"Checks if a testString is a substring of aString. The matching parts do not necessarily need to be consecutive, for example 'egli' matches 'renggli'."

	| index |
	index := 0.
	testString do: [ :char |
		index := aString
			findString: (String with: char)
			startingAt: index + 1
			caseSensitive: aBoolean.
		index = 0
			ifTrue: [ ^ false ] ].
	^ true