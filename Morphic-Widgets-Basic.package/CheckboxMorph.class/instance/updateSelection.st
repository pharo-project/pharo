updating
updateSelection
	"Update the selection state."

	self buttonMorph ifNotNil: [:m | m selected: self isSelected].
	self changed: #isSelected