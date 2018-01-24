Class {
	#name : #VSReleaseDevelopmentVersionCommandTest,
	#superclass : #TestCase,
	#instVars : [
		'classFactory',
		'project',
		'cmd'
	],
	#category : #'Versionner-Tests-Core-Commands'
}

{ #category : #'expected results' }
VSReleaseDevelopmentVersionCommandTest >> expectedTestResolveSymbolicVersionInResult [
	^ 'testVersion: spec
	<version: ''3.0'' imports: #(''1.0-baseline'')>
	spec
		for: #common
		do:
			[ spec blessing: #release.
			spec
				package: ''VersionnerTestXML-Parser''
					with: ''VersionnerTestXML-Parser-StephaneDucasse.142'';
				package: ''VersionnerTestXML-Tests-Parser''
					with:
					''VersionnerTestXML-Tests-Parser-StephaneDucasse.15'' ]'
]

{ #category : #'test data' }
VSReleaseDevelopmentVersionCommandTest >> generateTestVersion [
	| ast |

	ast := (project configurationClass methodNamed: #version101:) ast.
	ast selector: #testVersion:.
	(ast pragmas at: 1) arguments at: 1 put: (RBLiteralValueNode value: '3.0').
	project configurationClass compile: ast formattedCode
]

{ #category : #running }
VSReleaseDevelopmentVersionCommandTest >> setUp [
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
]

{ #category : #running }
VSReleaseDevelopmentVersionCommandTest >> tearDown [

	ASTCache reset.
	classFactory cleanUp.
	super tearDown	
]

{ #category : #tests }
VSReleaseDevelopmentVersionCommandTest >> testProjectReferenceConfigurationFrom [
	| config |
	
	config := cmd 
		projectReference: 'XMLWriter'
		configurationFrom: project devWorkflow developmentVersion.
	self 
		assert: config class
		equals: MetacelloMCProject.
	self 
		assert: config configuration class
		equals: ConfigurationOfVersionnerTestXMLWriter
]

{ #category : #tests }
VSReleaseDevelopmentVersionCommandTest >> testResolveSymbolicVersionsIn [
    | initialVersion configurationClass |
    self generateTestVersion.
    configurationClass := Smalltalk globals
        at: #ConfigurationOfVersionnerTestXMLParser.
    initialVersion := configurationClass project version: '3.0'.
    [ cmd resolveSymbolicVersionsIn: initialVersion ]
        on: MetacelloCannotUpdateReleasedVersionError
        do: [ :e |  ].
    self
        assert: (configurationClass methodNamed: #testVersion:) ast
        equals:
            (RBParser parseMethod: self expectedTestResolveSymbolicVersionInResult)
]
