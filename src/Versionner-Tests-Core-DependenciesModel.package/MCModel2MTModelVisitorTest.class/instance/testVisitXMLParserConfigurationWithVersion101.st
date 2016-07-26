tests
testVisitXMLParserConfigurationWithVersion101
	"test the visitor with the 1.38 version of the Versionner configuration "

	| visitor config project version |
	
	visitor := MCModel2MTModelVisitor new.
	
	config := 
	Smalltalk globals at: #ConfigurationOfVersionnerTestXMLParser ifAbsent: [ 
		classFactory duplicateClass: ConfigurationOfVersionnerTestXMLParserTemplate withNewName: 'ConfigurationOfVersionnerTestXMLParser'
	].
	version := config project version: '1.1'.
	project := visitor visitConfiguration: config withVersion: version.
	
	self assert: project class equals: MTProject.
	self assert: project name equals: 'VersionnerTestXMLParser'.
	self assert: project version name equals: '1.1'.
	self assert: project repository equals: 'http://www.smalltalkhub.com/mc/PharoExtras/XMLParser/main'.
	
	self checkGroups: project.
	self checkRequiredProjects: project.
	self checkPackages: project.