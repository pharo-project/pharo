installation
install: aProjectName version: aVersion
	"
		self install: 'Mustache' version: #stable
	"
	^self new refresh projects 
		detect: [ :each | each name = aProjectName ]
		ifFound: [:proj | proj installVersion: aVersion ]
		ifNone: [ self error: 'Project ', aProjectName , ' not found' ]