private
revertChange: aChange
	"Revert the change"

	| loader |
	loader := MCPackageLoader new.
	aChange inverse applyTo: loader.
	loader loadWithName: ChangeSet current name