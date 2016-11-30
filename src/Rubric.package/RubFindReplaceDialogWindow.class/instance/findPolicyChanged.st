updating
findPolicyChanged
	self changed: #findText.
	self changed: #caseSensitive.
	self changed: #isRegex.
	self changed: #entireWordsOnly.
	self changed: #findEnabled.
	self changed: #replaceText.
	self changed: #replaceEnabled.
	self changed: #replaceAllEnabled.
	self findTextFieldMorph textColor: self theme textColor

