view creation
open
	| window editor clsMorph mthMorph |
	window := (StandardWindow labelled: 'Method editor with shout') model: self.
	editor := self newScrolledTextMorph.
	clsMorph := self classSelectorMorph.
	clsMorph announcer when: RubTextChanged send: #whenClassNameChanged: to: self.
	mthMorph := self methodSelectorMorph.
	mthMorph announcer when: RubTextChanged send: #whenMethodNameChanged: to: self.
	editor
		hResizing: #spaceFill;
		vResizing: #spaceFill.
	window
		addMorph:
			(window
				newColumn:
					{(window
						newRow:
							{(clsMorph).
							(self metaSwitchMorph).
							(mthMorph).
							(self editButtonMorph )}).
					editor})
		fullFrame: LayoutFrame identity.
	^ window openInWorld