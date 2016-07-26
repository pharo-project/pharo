text-anchor
changeDocumentAnchor
	"Change the anchor from/to document anchoring"

	| newType |
	newType := self textAnchorType == #document 
		ifTrue: [#paragraph]
		ifFalse: [ #document].
	owner isTextMorph 
		ifTrue: 
			[owner 
				anchorMorph: self
				at: self position
				type: newType]