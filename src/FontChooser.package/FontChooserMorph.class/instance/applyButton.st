ui elements
applyButton
	^applyButton ifNil: [
		applyButton := (self
			newButtonFor: self
			action: #applyButtonClicked
			label: 'Apply' translated
			help: 'Click here to apply your selection without closing this dialog' translated)
			hResizing: #spaceFill ]