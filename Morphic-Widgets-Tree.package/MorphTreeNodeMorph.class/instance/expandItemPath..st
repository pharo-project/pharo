expanding-collapsing
expandItemPath: anArray
	"Open a path."
	
	| found |
	anArray isEmpty
		ifTrue: [^ container listManager setSelectedMorph: nil].
	found := nil.
	self
		withSiblingsDo: [:each | found
				ifNil: [(each complexContents withoutListWrapper = anArray first
							or: [anArray first isNil])
						ifTrue: [found := each]]].
	found
		ifNotNil: [found isExpanded
				ifFalse: [found toggleExpandedState].
			found changed.
			anArray size = 1
				ifTrue: [^ container listManager setSelectedMorph: found].
			^ found firstChild
				ifNil: [container setSelectedMorph: nil]
				ifNotNil: [found firstChild expandItemPath: anArray allButFirst]].
	^container setSelectedMorph: nil