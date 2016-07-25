*Polymorph-Widgets
openItemPath: anArray
	"Open a path based on wrapper item equivalence. Generally more specific
	than #openPath: (string based)."
	
	| found |
	anArray isEmpty
		ifTrue: [^ container setSelectedMorph: nil].
	found := nil.
	self
		withSiblingsDo: [:each | found
				ifNil: [(each complexContents withoutListWrapper == anArray first
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
				ifNotNil: [found firstChild openItemPath: anArray allButFirst]].
	^container setSelectedMorph: nil