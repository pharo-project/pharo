utilities
addExtensionClassAndMethodDefinitionsFromEntry: classEntry
	"A class entry here is a ZipDirectoryMember."

	"Would have been convenient to have directory members listing the files they contain."

	| classPropertiesDict entries |
	entries := self zip members select: [ :entry | entry fileName beginsWith: classEntry fileName ].
	(entries detect: [ :entry | self isPropertyFile: entry ] ifNone: [  ])
		ifNotNil: [ :propertyEntry | classPropertiesDict := MCFileTreeJsonParser parseStream: (self zipEntryContents: propertyEntry) ].
	self addMethodDefinitionsForClass: (classPropertiesDict at: 'name') in: entries root: classEntry