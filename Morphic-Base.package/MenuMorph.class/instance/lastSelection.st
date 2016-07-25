accessing
lastSelection
	"Return the label of the last selected item or nil."

	^selectedItem ifNotNil: [selectedItem selector].