accessing pattern
updateList
	"update the list by applying the pattern on the initial root item list"
	super rootItems: (self initialRootItems select: [ :p | (self pattern matchesIn: p name) notEmpty ]).
	super updateList