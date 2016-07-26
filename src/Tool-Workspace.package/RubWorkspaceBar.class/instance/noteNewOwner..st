submorphs-accessing
noteNewOwner: aMorph
	super noteNewOwner: aMorph.
	self color: self backgroundColor.
	self paragraphProvider beForSmalltalkScripting.
	wrappingPolicyMorph := self wrappingPolicyLabelMorph.
	editingModeMorph := self editingModeLabelMorph.
	lineNumbersDisplayMorph := self lineNumbersDisplayLabelMorph.
	row := self theme
		newRowIn: self
		for:
			{(self spacer: 2).
			editingModeMorph.
			(self spacer: 2).
			wrappingPolicyMorph.
			(self spacer: 2).
			lineNumbersDisplayMorph.
			(self spacer: 2)}.
	wrappingPolicyMorph on: #mouseDown send: #changeWrappingPolicy to: self.
	editingModeMorph on: #mouseDown send: #chooseEditingMode: to: self.
	lineNumbersDisplayMorph on: #mouseDown send: #changeLineNumbersDisplay to: self.
	row color: Color transparent.
	row hResizing: #spaceFill.
	self addMorph: row