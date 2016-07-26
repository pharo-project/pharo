expanding-collapsing
openNodePath: anArray 
	| found |
	anArray isEmpty
		ifTrue: [^ container listManager setSelectedMorph: nil].
	found := nil.
	self
		withSiblingsDo: [:each | found
				ifNil: [(each complexContents = anArray first
							or: [anArray first isNil])
						ifTrue: [found := each]]].
	found
		ifNotNil: [found isExpanded
				ifFalse: [found toggleExpandedState.
					container adjustSubmorphPositions].
			found changed.
			anArray size = 1
				ifTrue: [^ container setSelectedMorph: found].
			^ found firstChild
				ifNil: [container setSelectedMorph: nil]
				ifNotNil: [found firstChild openNodePath: anArray allButFirst]].
	^ container setSelectedMorph: nil