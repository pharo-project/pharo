cleanup
cleanUp: agressive
	"Remove empty categories when cleaning aggressively"

	agressive ifTrue: [SystemOrganization removeEmptyCategories].
