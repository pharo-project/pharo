metadata methods defintion
defineCatalogDescriptionTemplate
	self configurationMetaclass
		compile: 'catalogDescription
	"Returns a paragraph describing the project"
	^ ''MyProject is an amazing project that will change your life.'''
		classified: 'catalog'