private
initializeSelectors
	self resetSelectors.
	includeSelectors ifFalse: 
		[ Smalltalk keysAndValuesDo: [ :each :class |
			selectors add: (NECGlobalEntry contents: each) ].
		^ self ].
	narrowString ifEmpty: [ ^ self ].
	NECSymbols 
		contains: narrowString
		caseSensitive: NECPreferences caseSensitive
		do: [ :each | 
			(includeVariables or: [ each first isLowercase ]) ifTrue: 
				[ selectors add: (NECSelectorEntry 
						contents: each
						type: #selector).
				selectors size > self listLimit ifTrue: [ ^ self ]]]