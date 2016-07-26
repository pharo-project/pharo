brick-interactable-actions
uncheck
	"notifies #onUnchecked"
	self isChecked ifTrue: [
		isChecked := false.
		self announce: #onUnchecked.
		self changed ]