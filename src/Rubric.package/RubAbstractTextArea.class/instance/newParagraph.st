private
newParagraph
	| newParagraph |
	newParagraph := RubOpeningClosingDelimiterDecorator next: RubParagraph new.
	newParagraph textArea: self.
	newParagraph container: self compositionRectangle.
	^ newParagraph