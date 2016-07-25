private
analyze
	| index |
	index := MCDefinitionIndex definitions: additions.
	removals removeAllSuchThat: [:removal |
		(index definitionLike: removal
			ifPresent: [:addition | obsoletions at: addition put: removal]
			ifAbsent: []) notNil].
	super analyze