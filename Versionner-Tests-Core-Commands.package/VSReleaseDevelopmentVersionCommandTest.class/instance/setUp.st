running
setUp
	| visitor config version |
	super setUp.
	classFactory := ClassFactoryForTestCase new.
	
	visitor := MCModel2MTModelVisitor new.
	classFactory duplicateClass: ConfigurationOfVersionnerTestXMLParserTemplate withNewName: 'ConfigurationOfVersionnerTestXMLParser'.
	config := (Smalltalk globals at: #ConfigurationOfVersionnerTestXMLParser).
	version := config project version: '1.1'.
	project := visitor visitConfiguration: config withVersion: version.

	cmd := VSReleaseDevelopmentVersionCommand 
				target: project
				devWorkflow for: self.