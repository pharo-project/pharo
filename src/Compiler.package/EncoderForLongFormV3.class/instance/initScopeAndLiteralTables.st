initialize-release
initScopeAndLiteralTables
	super initScopeAndLiteralTables.
	"Start with an empty selector set to avoid the special selectors."
	selectorSet := Dictionary new: 16