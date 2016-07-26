structure
isInDockingBar
	"answer if the receiver is in a menu bar"
	^ (owner notNil) and: [owner isDockingBar]