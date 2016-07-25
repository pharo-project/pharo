metadata methods defintion
defineChangeLogTemplate

	self configurationMetaclass 
		compile: 'catalogChangeLog
	"Returns a paragraph describing the most important changes in the configuration class."
	^ ''- Version number - Date - topics
[[[
	ConfigurationOfXXX project version: ''''xx'''' ) load					
]]]
or simply
- Version number - Date - topics
- Version number - Date - topics
- Version number - Date - topics.'''
		classified: 'catalog'