utilities
addClassAndMethodDefinitionsFromEntry: classEntry
	"A class entry here is a ZipDirectoryMember."

	"Would have been convenient to have directory members listing the files they contain."

	| classPropertiesDict classComment entries |
	entries := self zip members
		select: [ :entry | entry isDirectory not and: [ entry fileName beginsWith: classEntry fileName ] ].
	(entries detect: [ :entry | self isPropertyFile: entry ] ifNone: [  ])
		ifNotNil: [ :propertyEntry | classPropertiesDict := MCFileTreeJsonParser parseStream: (self zipEntryContents: propertyEntry) ].
	(entries detect: [ :entry | entry splitFileName last = 'README.md' ] ifNone: [  ])
		ifNotNil: [ :commentEntry | classComment := (self zipEntryContents: commentEntry) contents withSqueakLineEndings ].
	self addClassDefinitionFrom: classPropertiesDict comment: classComment .
	self addMethodDefinitionsForClass: (classPropertiesDict at: 'name') in: entries root: classEntry