private
buildDropListFor: aSelector

	| dropList |
	dropList :=  self theme
					newDropListIn: World
					for: self
					list: #getPositionsList
					getSelected: #getSelectedPosition
					setSelected: #setSelectedPosition:
					getEnabled: nil
					useIndex: true
					help: 'Select the position where the plugin will be displayed'.
	dropList
		hResizing: #rigid.
	dropList width: 75.
	^ dropList