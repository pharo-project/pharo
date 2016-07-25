utilities
loadDefinitions
	"Query the zip archive for the relevant data."

	"Add support for scripts in packages."

	| query |
	definitions := OrderedCollection new.
	query := self zip
		membersMatching: packageDirectory , self slash , self monticelloMetaDirName , self slash , 'categories*'.
	query notEmpty
		ifTrue: [ self addDefinitionFromEntry: query first ]
		ifFalse: [ definitions add: (MCOrganizationDefinition categories: {(self packageNameFromPackageDirectory)}) ].
	self addClassAndMethodDefinitions.
	#('initializers*' 'preamble.st' 'postscript.st' 'preambleOfRemoval*' 'postscriptOfRemoval*')
		do: [ :item | 
			query := self zip membersMatching: packageDirectory , self slash , self monticelloMetaDirName , self slash , item.
			query notEmpty
				ifTrue: [ self addDefinitionFromEntry: query first ] ]