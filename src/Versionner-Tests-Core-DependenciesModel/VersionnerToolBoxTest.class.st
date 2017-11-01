"
test class for the versionner toolbox.
"
Class {
	#name : #VersionnerToolBoxTest,
	#superclass : #TestCase,
	#instVars : [
		'classFactory'
	],
	#category : 'Versionner-Tests-Core-DependenciesModel'
}

{ #category : #tests }
VersionnerToolBoxTest >> testProjectSpecFromRequiredProject [
	| spec |
	
	spec := VersionnerToolBox projectSpecFromRequiredProject: self versionnerTestXMLWriter.
	self assert: spec asString equals: self versionnerTestXMLWriterSpec
]

{ #category : #'test data' }
VersionnerToolBoxTest >> versionnerTestXMLWriter [
	^ MTDependantProject new
		name: 'VersionnerTestXMLWriter';
		configurationClass: ConfigurationOfVersionnerTestXMLWriter;
		displayName: 'aProjectAlias'
		yourself.
]

{ #category : #'expected results' }
VersionnerToolBoxTest >> versionnerTestXMLWriterSpec [
	^ 'spec 
	name: ''aProjectAlias'';
	projectReference: [
		spec
			className: #ConfigurationOfVersionnerTestXMLWriter;
			versionString: #''stable'' ].'
]
