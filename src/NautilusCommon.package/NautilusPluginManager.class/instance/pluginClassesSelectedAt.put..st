lists behavior
pluginClassesSelectedAt: anIndex put: aBoolean

	| object |
	object := self getPluginClassesList at: anIndex ifAbsent: [ ^ self ].
	^ pluginClassesSelected at: object put: aBoolean