updating
updateList
	|value|
	value := scrollBar value.
	self updateContentsWithPreviouslyExpanded: self currentlyExpanded.
	self vScrollValue: value