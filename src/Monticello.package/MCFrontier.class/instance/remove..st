advancing
remove: aVersionInfo
	frontier remove: aVersionInfo.
	aVersionInfo ancestors  do:
		[ :ancestor |
			bag remove: ancestor.
			(bag occurrencesOf: ancestor) = 0
				ifTrue: [frontier add: ancestor]].
	^aVersionInfo