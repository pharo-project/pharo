private
refreshExtraSelection
	| fullRefreshNeeded |
	fullRefreshNeeded := self useSelectionBar.
	self useFindReplaceSelection
		ifTrue: [ 
			fullRefreshNeeded := true.
			self findText isEmpty
				ifTrue: [ self paragraph findReplaceSelectionRegex: nil ]
				ifFalse: [ self paragraph findReplaceSelectionRegex: self findRegex ] ].
	self useSecondarySelection
		ifTrue: [ 
			fullRefreshNeeded := true.
			(self paragraph secondarySelection isNil and: [ self selection isEmptyOrNil not ])
				ifTrue: [ self paragraph secondarySelection: (self text asString allRangesOfSubstring: self selection asString) ] ].
	fullRefreshNeeded
		ifTrue: [ 
			self paragraph extraSelectionChanged.
			"> Alain: should be that : 
			 > self paragraph extraSelectionRects do: [:r | self invalidRect: r]
			 > but it doesn't work"
			self invalidRect: self bounds ]