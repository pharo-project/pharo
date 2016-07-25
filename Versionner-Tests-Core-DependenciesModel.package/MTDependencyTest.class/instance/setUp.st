as yet unclassified
setUp
	| visitor |
	
	visitor := MCModel2MTModelVisitor new.
	version := ConfigurationOfVersionnerTestXMLWriter project version: '2.1.0'.
	project := visitor visitConfiguration: ConfigurationOfVersionnerTestXMLWriter withVersion: version.
