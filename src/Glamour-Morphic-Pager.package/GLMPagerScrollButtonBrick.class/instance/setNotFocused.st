as yet unclassified
setNotFocused

	focused ifTrue: [
		self checkedImage: checkedIcon.
		self uncheck.
		focused := false ]