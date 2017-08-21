accessing
value: aTraitAlias
	super value:aTraitAlias.
	value := aTraitAlias aliases collect:#asExportedLiteral.
	