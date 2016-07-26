brick-interactable-actions
select
	"notifies #onMouseHover"
	self isSelected ifFalse: [ 
		isSelected := true.
		self announce: #onMouseHover.
		self changed ]