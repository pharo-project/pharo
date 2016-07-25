accessing
setText: aText from: aView
	self replaceTextWith: aText.
	self announcer announce: RubTextUpdatedInModel.
	^ true