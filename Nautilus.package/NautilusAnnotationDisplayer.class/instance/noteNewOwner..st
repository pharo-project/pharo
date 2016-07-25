submorphs-accessing
noteNewOwner: aMorph
	super noteNewOwner: aMorph.
	self color: self backgroundColor.
	self paragraphProvider announcer when: MorphGotFocus send: #updateContents to: self.
	lineAnnotation := self lineAnnotationMorph.
	wrappingPolicyMorph := self wrappingPolicyLabelMorph.
	lineNumbersDisplayMorph := self lineNumbersDisplayLabelMorph.
	formatAsYouReadMorph := self formatAsYouReadMorph.
	row := self theme
		newRowIn: self
		for:
			{self spacer: 2.
			lineAnnotation.
			self spacer: 2.
			formatAsYouReadMorph.
			self spacer: 2.
			wrappingPolicyMorph.
			self spacer: 2.
			lineNumbersDisplayMorph.
			self spacer: 2}.	
	wrappingPolicyMorph on: #mouseDown send: #changeWrappingPolicy to: self.
	lineNumbersDisplayMorph on: #mouseDown send: #changeLineNumbersDisplay to: self.
	row color: Color transparent.
	row hResizing: #spaceFill.
	lineAnnotation hResizing: #spaceFill.
	self addMorph: row