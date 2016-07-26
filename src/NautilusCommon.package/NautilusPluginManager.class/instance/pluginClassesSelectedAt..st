lists behavior
pluginClassesSelectedAt: anIndex

	| object |
	object := self getPluginClassesList at: anIndex ifAbsent: [ ^ false ].
	^ pluginClassesSelected at: object ifAbsent: [ false ]