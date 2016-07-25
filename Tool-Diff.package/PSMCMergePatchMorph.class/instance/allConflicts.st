as yet unclassified
allConflicts
	"Answer all conflicts in the model."

	^self model select: [:op | op isConflict]