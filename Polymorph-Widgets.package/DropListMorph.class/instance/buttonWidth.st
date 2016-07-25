private
buttonWidth
	"Answer based on scrollbar size."
	
	^ (self theme scrollbarThickness + 3)
		max: self theme dropListControlButtonWidth