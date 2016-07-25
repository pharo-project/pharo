text-anchor
changeParagraphAnchor
	"Change the anchor from/to paragraph anchoring"

	| newType |
	newType := self textAnchorType == #paragraph 
		ifTrue: [#document]
		ifFalse: [#paragraph].
	owner isTextMorph 
		ifTrue: 
			[owner 
				anchorMorph: self
				at: self position
				type: newType]