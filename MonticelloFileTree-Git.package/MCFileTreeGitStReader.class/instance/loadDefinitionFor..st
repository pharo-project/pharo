utilities
loadDefinitionFor: aMCDefinition
	"Load the definition in this commit for that MCDefinition. Return nil if it is empty (effect of git tracking renames :))"

	| classPropertiesDict entries classEntry |
	definitions := OrderedCollection new.
	self zipForDefinition: aMCDefinition.
	classEntry := packageDirectory , self slash , aMCDefinition className , '.class/'.
	entries := self zip members select: [ :entry | entry isDirectory not and: [ entry fileName beginsWith: classEntry ] ].
	entries isEmpty
		ifTrue: [ ^ nil ].
	classPropertiesDict := Dictionary new.
	(entries detect: [ :entry | entry splitFileName last = 'properties.json' ] ifNone: [  ])
		ifNotNil: [ :propertyEntry | classPropertiesDict := MCFileTreeJsonParser parseStream: (self zipEntryContents: propertyEntry) ].
	self
		addMethodDefinitionsForClass: (classPropertiesDict at: 'name')
		in: entries
		root: (self zip members detect: [ :entry | entry isDirectory and: [ entry fileName endsWith: classEntry ] ]).
	^ definitions