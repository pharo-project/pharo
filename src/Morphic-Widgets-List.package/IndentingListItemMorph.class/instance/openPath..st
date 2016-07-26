private-container protocol
openPath: anArray 
	| found |
	anArray isEmpty
		ifTrue: [^ container setSelectedMorph: nil].
	found := self findPathIn: anArray.
	found
		ifNil: [^ container setSelectedMorph: nil].
	found isExpanded
		ifTrue: [found refreshExpandedState]
		ifFalse: [found toggleExpandedState].
	container adjustSubmorphPositions.
	found changed.
	anArray size = 1
		ifTrue: [^ container setSelectedMorph: found].
	^ found firstChild
		ifNil: [container setSelectedMorph: nil]
		ifNotNil: [found firstChild openPath: anArray allButFirst]