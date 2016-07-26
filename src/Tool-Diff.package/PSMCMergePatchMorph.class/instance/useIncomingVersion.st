as yet unclassified
useIncomingVersion
	"Mark the conflict as remote."
	
	self selectedChangeWrapper chooseRemote.
	self changed: #changes.
	self updateSource