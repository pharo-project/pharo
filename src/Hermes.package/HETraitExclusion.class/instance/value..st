accessing
value: aTraitExclusion
	super value: aTraitExclusion.
	value := aTraitExclusion exclusions collect:#asExportedLiteral.