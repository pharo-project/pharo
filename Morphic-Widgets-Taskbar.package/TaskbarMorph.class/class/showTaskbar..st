setting
showTaskbar: aBoolean
	ShowTaskbar ~= aBoolean
		ifTrue: [ShowTaskbar := aBoolean.
			self showTaskbarPreferenceChanged]