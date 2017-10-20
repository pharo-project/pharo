nested dictionaries
at: firstKey at: secondKey ifAbsentPut: aZeroArgBlock
	"Return the object stored in the second dictionary at secondKey. The second dictionary is accessed via the key firstKey. If firstKey is not defined, set a new dictionary for the second key and set the value of aZeroArgBlock execution. If firstKey is defined and not second key set the value of aZeroArgBlock execution. See NestedDictionaryTest for examples."

	| subDictionary |
	subDictionary := self at: firstKey ifAbsentPut: [ Dictionary new ].
	^ subDictionary at: secondKey ifAbsentPut: aZeroArgBlock