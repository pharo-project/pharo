controls
newNoButtonFor: aModel
	"Answer a new No button."

	^self theme
		newNoButtonIn: self
		for: aModel