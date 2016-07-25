actions
addDefinitionsTo: aCollection
	| tokens definition |
	tokens := source parseLiterals.
	definition := MCScriptDefinition
		scriptSelector: tokens second allButLast
		script: tokens third
		packageName: tokens first third.
	aCollection add: definition.