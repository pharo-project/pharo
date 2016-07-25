geometry
bounds: newBounds
	"Make sure position: gets called before extent:; Andreas' optimization for growing/shrinking in ChangeSet 3119 screwed up selection of morphs from underlying pasteup."

	selectedItems := OrderedCollection new.  "Avoid repostioning items during super position:"
	self position: newBounds topLeft; extent: newBounds extent
