morph creation
newWindowIn: aThemedMorph for: aModel title: titleString
	"Answer a new StandardWindow with given model and title."

	^StandardWindow new
		title: titleString;
		model: aModel;
		yourself