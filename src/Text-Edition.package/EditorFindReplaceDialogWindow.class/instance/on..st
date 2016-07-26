updating
on: aTextView
	(model isNil or: [model  ~= aTextView])
		ifTrue: [self findText: self findText.
			self model: aTextView.
			self changed: #findText.
			self changed: #caseSensitive.
			self changed: #replaceEnabled.
			self owner ifNotNil: [self findText: self findText]].
