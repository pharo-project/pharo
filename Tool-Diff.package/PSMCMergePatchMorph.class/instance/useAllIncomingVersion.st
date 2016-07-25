as yet unclassified
useAllIncomingVersion
	"Mark all conflicts as remote."
	
	self allConflicts do: [:c | c chooseRemote].
	self changed: #changes.
	self updateSource