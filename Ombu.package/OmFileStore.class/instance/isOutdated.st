testing
isOutdated
	"Answer if I'm out of sync respect to the file."

	^ self copyReopened refresh entriesCount ~= self entriesCount