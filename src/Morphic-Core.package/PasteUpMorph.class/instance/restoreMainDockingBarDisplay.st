world state
restoreMainDockingBarDisplay
	"Restore the display of docking bars"
	self dockingBars
		do: [:each | each updateBounds]