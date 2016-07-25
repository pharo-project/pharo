brick-interactable-actions
check
	"notifies #onChecked"
	self isChecked ifFalse: [
		isChecked := true.
		self announce: #onChecked.
		self changed ]