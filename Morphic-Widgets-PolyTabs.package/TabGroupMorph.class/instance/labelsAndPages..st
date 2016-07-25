page
labelsAndPages: assocs
	"Replace the tabs and the associated pages."

	self contentMorph removeAllMorphs.
	self tabSelectorMorph removeAllMorphs.
	assocs do: [:a | self addPage: a value label: a key]