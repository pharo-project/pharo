public
openFromRubric: aMorph withResult: anObject
	| string |
	string := anObject printString.
	self withString: string from: aMorph.
	self width: self width + inspectButton width.
	textMorph textArea
		on: $i meta
		do: [ anObject inspect ].
	textMorph textArea
		on: Character cr asKeyCombination
		do: [ 
			self delete.
			aMorph textArea removeHighlightSegment.
			aMorph editor atEndOfLineInsertAndSelect: string asComment.
			aMorph takeKeyboardFocus ].
	textMorph textArea
		on: $p meta
		do: [ 
			self delete.
			aMorph textArea removeHighlightSegment.
			aMorph editor atEndOfLineInsertAndSelect: string.
			aMorph takeKeyboardFocus ].
	inspectButton addUpAction: [ anObject inspect ].
	self openInWorld