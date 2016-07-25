running
setUp
	"Create a model of the project version we want to work on."
	| version |
	
	"create a copy of the class to don't pollute the configuration with code generated for tests purposes"
	classFactory := ClassFactoryForTestCase new.
	class := classFactory duplicateClass: ConfigurationOfVersionnerTestXMLParserTemplate withNewName: #ConfigurationOfVersionnerTestXMLParser.
	
	visitor := MCModel2MTModelVisitor new.
	version := class project version: '1.1'.
	project := visitor visitConfiguration: class withVersion: version.
		