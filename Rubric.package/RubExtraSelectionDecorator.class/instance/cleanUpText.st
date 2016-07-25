computation
cleanUpText
	self text removeAttribute: RubTextSelectionColor findReplaceSelection.
	self text removeAttribute: RubTextSelectionColor secondarySelection.
