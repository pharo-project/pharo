This is a framework class that can be used by clients that create custom browsers containing text presentations with custom highlighters.

Usage example:

GLMRubricHighlightedTextPresentation new
	editingMode: [ 
		GLMHighlighterTextRubEditingMode withStylerDecorator: 
			GLMHighlighterTextStylerDecorator new ].