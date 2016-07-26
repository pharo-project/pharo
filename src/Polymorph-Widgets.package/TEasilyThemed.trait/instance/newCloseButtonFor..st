controls
newCloseButtonFor: aModel
	"Answer a new close button."

	^self theme
		newCloseButtonIn: self
		for: aModel