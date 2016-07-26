private
addToEntries: aCollection 
	| temp caseSensitive |
	caseSensitive := NECPreferences caseSensitive.
	temp := aCollection select: [ :each | 
		each contents 
			occursInWithEmpty: narrowString
			caseSensitive: caseSensitive ].
	entries addAll: temp