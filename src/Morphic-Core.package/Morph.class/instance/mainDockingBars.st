submorphs-accessing
mainDockingBars
	"Answer the receiver's main dockingBars"
	^ self dockingBars
		select: [:each | each hasProperty: #mainDockingBarTimeStamp]