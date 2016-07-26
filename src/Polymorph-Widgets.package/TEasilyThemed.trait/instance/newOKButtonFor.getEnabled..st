controls
newOKButtonFor: aModel getEnabled: enabledSel
	"Answer a new OK button."

	^self theme
		newOKButtonIn: self
		for: aModel
		getEnabled: enabledSel