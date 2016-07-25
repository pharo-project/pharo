controls
newCancelButtonFor: aModel
	"Answer a new cancel button."

	^self theme
		newCancelButtonIn: self
		for: aModel