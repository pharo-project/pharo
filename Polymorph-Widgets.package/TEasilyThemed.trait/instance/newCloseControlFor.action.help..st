controls
newCloseControlFor: aModel action: aValuable help: helpText
	"Answer a new cancel button."

	^self theme
		newCloseControlIn: self
		for: aModel
		action: aValuable
		help: helpText