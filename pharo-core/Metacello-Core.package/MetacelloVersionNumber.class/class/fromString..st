instance creation
fromString: aString

	| new components |
	components := OrderedCollection new.
	(aString findTokens: '.') do: [:subString | | strs |
		strs := subString findTokens: '-'.
		"first subString token could be an integer"
		components add: (self extractNumericComponent: strs first).
		strs size > 1
			ifTrue: [
				"remaining are uncoditionally Strings, because of leading $-"
				components addAll: strs allButFirst ]].
	new := self new: components size.
	1 to: components size do: [:i | new at: i put: (components at: i) ].
	^new