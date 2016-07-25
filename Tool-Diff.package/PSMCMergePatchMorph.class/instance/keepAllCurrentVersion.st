as yet unclassified
keepAllCurrentVersion
	"Mark all conflicts as local."
	
	self allConflicts do: [:c | c chooseLocal].
	self changed: #changes.
	self updateSource