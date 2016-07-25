updating
findPolicyChanged
	self changed: #findText.
	self findTextFieldMorph textColor: Color  black.
	self changed: #caseSensitive.
	self changed: #isRegex.
	self changed: #entireWordsOnly.
	self changed: #findEnabled.
	self changed: #replaceText.
	self changed: #replaceEnabled.
	self changed: #replaceAllEnabled.
	self model ifNotNil: [self model selectionChanged].
