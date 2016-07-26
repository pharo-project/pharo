utilities
addTraitAndMethodDefinitionsFromEntry: traitEntry
	| classPropertiesDict classComment entries |
	entries := self zip members
		select: [ :entry | entry isDirectory not and: [ entry fileName beginsWith: traitEntry fileName ] ].
	(entries detect: [ :entry | self isPropertyFile: entry ] ifNone: [  ])
		ifNotNil: [ :propertyEntry | classPropertiesDict := MCFileTreeJsonParser parseStream: (self zipEntryContents: propertyEntry) ].
	(entries detect: [ :entry | entry splitFileName last = 'README.md' ] ifNone: [  ])
		ifNotNil: [ :commentEntry | classComment := (self zipEntryContents: commentEntry) contents ]
		ifNil: [ classComment := '' ].
	self addTraitDefinitionFrom: classPropertiesDict comment: classComment withSqueakLineEndings.
	self addMethodDefinitionsForClass: (classPropertiesDict at: 'name') in: entries root: traitEntry