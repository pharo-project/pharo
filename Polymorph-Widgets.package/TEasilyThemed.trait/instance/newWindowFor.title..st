controls
newWindowFor: aModel title: titleString
	"Answer a new window morph."
	
	^self theme
		newWindowIn: self
		for: aModel
		title: titleString