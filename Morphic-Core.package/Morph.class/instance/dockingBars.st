submorphs-accessing
dockingBars
	"Answer the receiver's dockingBars"
	^ self submorphs
		select: [:each | each isDockingBar]
