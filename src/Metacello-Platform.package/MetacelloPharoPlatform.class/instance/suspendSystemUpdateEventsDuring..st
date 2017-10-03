system
suspendSystemUpdateEventsDuring: aBlock
	"Wraps call to the system evetn manager to tell it that we want to suspend events during execution of aBlock"
	"From Pharo2, we cannot suspend system events since missing events brak the image (rpackage at least should be notified."
	"SystemAnnouncer uniqueInstance suspendAllWhile: aBlock"
	^ aBlock value
