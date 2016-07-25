updating
openItemPath: anArray
	"Open a path based on wrapper item equivalence. Generally more specific
	than #openPath: (string based)."
	
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
				ifNotNil: [found firstChild openItemPath: anArray allButFirst]].
	^self