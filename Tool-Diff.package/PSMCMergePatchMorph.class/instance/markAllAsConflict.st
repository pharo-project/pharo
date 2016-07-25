as yet unclassified
markAllAsConflict
	"Mark all conflicts as unresolved."
	
	self allConflicts do: [:c | c clearChoice].
	self changed: #changes.
	self updateSource