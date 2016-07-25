events
deselectTimeOut: evt
	"Deselect timout. Now really deselect"

	owner selectedItem == self
		ifTrue: [ 
			evt hand releaseMouseFocus: owner.
			owner selectItem: nil event: evt ]