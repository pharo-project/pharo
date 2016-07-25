controls
newMenuFor: aModel
	"Answer a new menu."

	^self theme
		newMenuIn: self
		for: aModel