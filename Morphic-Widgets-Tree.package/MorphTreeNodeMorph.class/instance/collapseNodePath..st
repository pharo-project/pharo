expanding-collapsing
collapseNodePath: anArray
	"Close a path based on node."
	
	| found |
	anArray isEmpty
		ifTrue: [^ container setSelectedMorph: nil].
	found := nil.
	self
		withSiblingsDo: [:each | found
				ifNil: [(each complexContents = anArray first)
						ifTrue: [found := each]]].
	found
		ifNotNil: [(found isExpanded and: [anArray size = 1])
				ifTrue: [found toggleExpandedState.
					container adjustSubmorphPositions].
			found changed.
			anArray size = 1
				ifTrue: [^ container listManager setSelectedMorph: found].
			^ found firstChild
				ifNil: [container setSelectedMorph: nil]
				ifNotNil: [found firstChild collapseNodePath: anArray allButFirst]].
	^container setSelectedMorph: nil