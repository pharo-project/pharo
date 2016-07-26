source code management
getSourceReplacingSelectorWith: newSelector
	| oldKeywords newKeywords source newSource oldSelector start |
	source := self sourceCode.
	oldSelector := self selector.
	oldSelector = newSelector ifTrue: [ ^ source ].
	
	oldKeywords := oldSelector keywords.
	newKeywords := (newSelector ifNil: [self defaultSelector]) keywords.
	[oldKeywords size = newKeywords size] assert.
	
	newSource := source.
	start := 1.
	oldKeywords with: newKeywords do: [:oldKey :newKey| |pos|
		pos := newSource findString: oldKey startingAt: start .
		newSource := newSource copyReplaceFrom: pos to: (pos + oldKey size -1) with: newKey.
		start := pos + newKey size ].

	^newSource