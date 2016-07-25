instance creation
newChangeTreeMorph
	"Answer a new morph for the tree of changes."

	^(self
		newTreeFor: self
		list: #changes
		selected: #selectedChangeWrapper
		changeSelected: #selectedChangeWrapper:)
		getMenuSelector: #changesMenu:;
		keystrokeActionSelector: #changeTreeKey: