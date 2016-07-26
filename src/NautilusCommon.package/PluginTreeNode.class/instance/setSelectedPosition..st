private
setSelectedPosition: anIndex

	| position |
	selectedPosition := anIndex.
	position := self getPositionsList at: anIndex.
	self item at: 2 put: position.
	self changed: #getSelectedPosition