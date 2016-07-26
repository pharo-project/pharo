accessing
migrateProgressWhileUpdatingBounds: aBlockChangingBounds
	"Keep the progress value consistent while we change min / max"
	| progress |
	progress := self progress.
	aBlockChangingBounds value.
	self progress: progress.