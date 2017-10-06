building
newListMorph
	"Answer a new drop-list morph."

	^self
		newDropListFor: self
		list: #list
		getSelected: #selectionIndex
		setSelected: nil
		getEnabled: nil
		help: nil