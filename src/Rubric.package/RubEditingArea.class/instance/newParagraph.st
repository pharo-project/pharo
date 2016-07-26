private
newParagraph
	^ RubCurrentLineBarDecorator	next: (RubExtraSelectionDecorator next: super newParagraph)