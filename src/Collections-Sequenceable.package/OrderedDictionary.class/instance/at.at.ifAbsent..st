nested dictionaries
at: firstKey at: secondKey ifAbsent: aZeroArgBlock
		"Return the object stored in the second dictionary at secondKey. The second dictionary is accessed via the key firstKey. Execute aZeroArgBlock in case one of the key is wrong."
		
	| subDictionary |
	subDictionary := self at: firstKey ifAbsent: [ ^ aZeroArgBlock value ].
	^ subDictionary at: secondKey ifAbsent: aZeroArgBlock