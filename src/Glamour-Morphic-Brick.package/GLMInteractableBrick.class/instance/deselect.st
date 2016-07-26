brick-interactable-actions
deselect
	"notifies #onMouseUnhover"
	self isSelected ifTrue: [
		isSelected := false.
		self announce: #onMouseUnhover.
		self changed ]