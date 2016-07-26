events
deselectTimeOut: evt
	"Deselect timout. Now really deselect"

	owner selectedItem == self
		ifTrue: [ 
			evt hand newMouseFocus: owner.
			owner selectItem: nil event: evt ]