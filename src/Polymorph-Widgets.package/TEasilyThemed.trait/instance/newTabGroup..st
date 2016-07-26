controls
newTabGroup: labelsAndPages
	"Answer a tab group with the given tab labels associated with pages."

	^self theme
		newTabGroupIn: self
		for: labelsAndPages