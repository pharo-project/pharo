accessing
findText: aStringOrText
	findText := aStringOrText asText.
	self findPolicyChanged.
	self textArea whenFindTextChanged: RubFindTextChanged new.
	^ true