tests
testProjectSpecFromRequiredProject
	| spec |
	
	spec := VersionnerToolBox projectSpecFromRequiredProject: self versionnerTestXMLWriter.
	self assert: spec asString equals: self versionnerTestXMLWriterSpec