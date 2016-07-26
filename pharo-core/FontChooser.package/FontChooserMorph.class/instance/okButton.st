ui elements
okButton

	okButton ifNotNil: [ ^ okButton ].
	okButton := self
			newButtonFor: self
			action: #okButtonClicked
			label: 'OK' translated
			help: 'Click here to close this dialog, and accept your selection' translated.
		^ okButton
			hResizing: #spaceFill;
			yourself.