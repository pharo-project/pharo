ui-building
nextPrefixWith: aPrefix
	| curr pref found lab |
	curr := aPrefix size.
	lab := self labels first.
	found := false.
	[lab size <= curr 
		ifTrue: [found := true]
		ifFalse: [pref := lab copyFrom: 1 to: curr.
			self labels 
				do: [:l | found 
					ifFalse: [(l asString beginsWith: pref)
						ifFalse: [pref := pref allButLast.
							found := true]]]].
	found] whileFalse: [curr := curr + 1].
	^ (pref notNil and: [pref size > aPrefix size])
		ifTrue: [pref]
