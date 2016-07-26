utilities
loadDefinitions
    | entries directory |
    definitions := OrderedCollection new.
    directory := self fileUtils directoryFromPath: self monticelloMetaDirName relativeTo: packageDirectory.
    (self fileUtils directoryExists: directory)
        ifTrue: [ 
            		entries := directory entries.
            		self
                			addDefinitionFromFile: (entries detect: [ :entry | entry name beginsWith: 'categories' ] ifNone: [  ])
                			inDirectory: directory ]
	ifFalse: [definitions add: (MCOrganizationDefinition categories: {self packageNameFromPackageDirectory }) ].
    self addClassAndMethodDefinitionsFromDirectory: packageDirectory.
    (self fileUtils directoryExists: directory)
        ifTrue: [ 
            self
                addDefinitionFromFile: (entries detect: [ :entry | entry name beginsWith: 'initializers' ] ifNone: [  ])
                inDirectory: directory ]