Class {
	#name : #MCModel2MTModelVisitorTest,
	#superclass : #RPackageTestCase,
	#instVars : [
		'classFactory'
	],
	#category : #'Versionner-Tests-Core-DependenciesModel'
}

{ #category : #private }
MCModel2MTModelVisitorTest >> checkGroups: project [
	| groups group |
	
	groups := project groups sorted: [ :a :b | a name <= b name].
	
	self assert: groups size = 3.
	
	self assert: (groups collect: #name) asArray = #('Core' 'Tests' 'default').
	
	group := groups at: 1.
	self assert: group notNil.
	self assert:  group name equals: 'Core'.
	self assert: (group dependencies hasEqualElements: #('VersionnerTestXML-Parser')).
		
	group := groups at: 2.
	self assert: group notNil.
	self assert:  group name equals: 'Tests'.
	self assert: (group dependencies hasEqualElements: #('VersionnerTestXML-Tests-Parser')).
	
	group := groups at: 3.
	self assert: group notNil.
	self assert:  group name equals: 'default'.
	self assert: (group dependencies hasEqualElements: #('Core' 'Tests')).
]

{ #category : #private }
MCModel2MTModelVisitorTest >> checkPackages: project [
	| packages proj |
	
	packages := project packages.
	
	proj := packages at: 1.
	self assert: proj notNil.
	self assert:  proj name equals: 'VersionnerTestXML-Parser'.
	self assert: proj dependencies equals: (OrderedCollection newFrom: #('BitmapCharacterSet' 'XMLWriter')).
	
	proj := packages at: 2.
	self assert: proj notNil.
	self assert:  proj name equals: 'VersionnerTestXML-Tests-Parser'.
	self assert: proj dependencies equals: (OrderedCollection newFrom: #('VersionnerTestXML-Parser')).

]

{ #category : #private }
MCModel2MTModelVisitorTest >> checkRequiredProjects: project [
	| requiredProjects proj |
	
	requiredProjects := project requiredProjects.
	
	proj := requiredProjects at: 1.
	self assert: proj notNil.
	self assert:  proj displayName equals: 'BitmapCharacterSet'.
	self assert:  proj name equals: 'VersionnerTestBitmapCharacterSet'.
	self assert: proj version name equals: 'stable'.
	
	proj := requiredProjects at: 2.
	self assert: proj notNil.
	self assert: proj displayName equals: 'XMLWriter'.
	self assert: proj name equals: 'VersionnerTestXMLWriter'.
	self assert: proj version name equals: 'stable'.

]

{ #category : #running }
MCModel2MTModelVisitorTest >> setUp [
	super setUp.
	classFactory := ClassFactoryForTestCase new.
]

{ #category : #running }
MCModel2MTModelVisitorTest >> tearDown [
	
	classFactory cleanUp.
	super tearDown
]

{ #category : #tests }
MCModel2MTModelVisitorTest >> testVisitXMLParserConfigurationWithVersion101 [
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
]
