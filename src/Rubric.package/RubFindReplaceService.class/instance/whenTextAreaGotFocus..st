events handling
whenTextAreaGotFocus: anAnnounce
	self textArea: anAnnounce morph.
	self findText: self findText.
	self changed: #findText.
	self changed: #caseSensitive.
	self changed: #replaceEnabled.
