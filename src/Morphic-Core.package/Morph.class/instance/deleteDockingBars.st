submorphs-add/remove
deleteDockingBars
	"Delete the receiver's docking bars"
	self dockingBars
		do: [:each | each delete]