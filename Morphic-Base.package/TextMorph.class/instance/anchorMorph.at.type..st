anchors
anchorMorph: aMorph at: aPoint type: anchorType
	| relPt index newText block |
	aMorph owner == self ifTrue:[self removeMorph: aMorph].
	aMorph textAnchorType: nil.
	aMorph relativeTextAnchorPosition: nil.
	self addMorphFront: aMorph.
	aMorph textAnchorType: anchorType.
	aMorph relativeTextAnchorPosition: nil.
	anchorType == #document ifTrue:[^self].
	relPt := self transformFromWorld globalPointToLocal: aPoint.
	index := (self paragraph characterBlockAtPoint: relPt) stringIndex.
	newText := Text string: (String value: 1) attribute: (TextAnchor new anchoredMorph: aMorph).
	anchorType == #inline ifTrue:[
		self paragraph replaceFrom: index to: index-1 with: newText displaying: false.
	] ifFalse:[
		index := index min: paragraph text size.
		index := paragraph text string lastIndexOf: Character cr startingAt: index ifAbsent:[0].
		block := paragraph characterBlockForIndex: index+1.
		aMorph relativeTextAnchorPosition: (relPt x - bounds left) @ (relPt y - block top ).
		self paragraph replaceFrom: index+1 to: index with: newText displaying: false.
	].
	self fit.