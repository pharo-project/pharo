"
This class is used to manage the development workflow for a dedicated project.

"
Class {
	#name : #MTDevelopmentWorkfow,
	#superclass : #Object,
	#instVars : [
		'project'
	],
	#category : 'Versionner-Core-DependenciesModel'
}

{ #category : #initialization }
MTDevelopmentWorkfow class >> newWithProject: project [
	"Create a new instance dedicated to the project given as parameter."

	^ self new
		project: project;
		yourself
]

{ #category : #private }
MTDevelopmentWorkfow >> createBaseline [
	"Create a new baseline with information given by the project" 
	|lastBaseline baselineVersionString|
	
	lastBaseline := self configurationClass baselines detect: [:aBaseline | aBaseline isBleedingEdge].
	baselineVersionString := (lastBaseline name asInteger +1) asString.
	
	^ self createBaseline: baselineVersionString.
]

{ #category : #private }
MTDevelopmentWorkfow >> createBaseline: baselineVersionString [
	"Create a new baseline with information given by the project"
	MetacelloToolBox
		createBaseline: baselineVersionString
		for: project configurationClass name
		repository: project repository
		requiredProjects: (project requiredProjects collect: [ :each | each name ])
		packages: (project packages collect: [ :aPackage | aPackage name ])
		repositories: #()
		dependencies: project dependenciesMap
		groups: (project groups collect: [ :aGroup | aGroup name -> aGroup dependencies ])
		requiredProjectSpecs: self requiredProjectSpecs
]

{ #category : #protocol }
MTDevelopmentWorkfow >> createInitialDevelopment [
	"Create an initial development version for an empty configuration."

	^ self createInitialDevelopment: '0.1-baseline'
]

{ #category : #private }
MTDevelopmentWorkfow >> createInitialDevelopment: versionString [
	"Create an initial development version with an empty configuration and return the configuration class."

	| toolbox |
	toolbox := MetacelloToolBox configurationNamed: project name.	
		
	"Only a static method available on the toolbox to create baselines"
	MetacelloToolBox
		createBaseline: versionString 
		for: toolbox project configuration class name
		repository: project repository
		requiredProjects: #()
		packages: #()
		repositories: #()
		dependencies: #()
		groups: #().
		
	toolbox
		symbolicVersionMethod: #development;
		addSymbolicSection: #common version: versionString;
		commitMethod.
		
	^toolbox project configuration class
]

{ #category : #protocol }
MTDevelopmentWorkfow >> createNextDevelopment: newDevVersionString [
	"Create the next baseline that will be used as development version."

	self createBaseline: newDevVersionString.
	self
		developmentVersion: newDevVersionString
		platformAttribute: #common.
]

{ #category : #protocol }
MTDevelopmentWorkfow >> developmentVersion [
	^ project configurationClass project version: #development
]

{ #category : #private }
MTDevelopmentWorkfow >> developmentVersion: devVersionString platformAttribute: platformAttribute [
	"Set the #development symbolic version to the given version, using the given platformAttribute."
	
	(MetacelloToolBox configurationNamed: project name)
		symbolicVersionMethod: #development;
		addSymbolicSection: platformAttribute version: devVersionString;
		commitMethod.

]

{ #category : #private }
MTDevelopmentWorkfow >> developmentVersionNumber: aDevelopmentVersionString [
	| versionNumber |
	versionNumber := aDevelopmentVersionString copyReplaceAll: 'baseline' with: ''.
	versionNumber  :=  versionNumber copyReplaceAll: '-' with: ''.
	^ versionNumber
]

{ #category : #private }
MTDevelopmentWorkfow >> isDevelopmentUsedInRelease [
	| devVersion allProjectVersions |
	
	devVersion := self developmentVersion.
	devVersion ifNil: [ ^ false ].
	(devVersion blessing = #baseline) ifFalse: [ ^false ].
	
	allProjectVersions := project configurationClass project versions.
	^ allProjectVersions 
		detect: [ :aVersion | aVersion importedVersions includes: devVersion versionString ]
		ifFound: [ true ]
		ifNone: [ false ]
]

{ #category : #protocol }
MTDevelopmentWorkfow >> nextDevelopmentVersionString [
	| newVersionString |
	
	newVersionString := self developmentVersion versionString.
	[ project configurationClass project hasVersion: newVersionString ] whileTrue: [
		newVersionString := self nextDevelopmentVersionString: newVersionString ].
	^ newVersionString
]

{ #category : #private }
MTDevelopmentWorkfow >> nextDevelopmentVersionString: aDevelopmentVersionString [
	| versionNumber lastPointIndex newVersionNumber |
	versionNumber := self developmentVersionNumber: aDevelopmentVersionString.
	lastPointIndex := versionNumber 
						lastIndexOf: $. 
						ifAbsent: [ nil ].
	newVersionNumber := lastPointIndex 
		ifNil: [ versionNumber + 1 ]
		ifNotNil: [ | newMinorVersion |
				newMinorVersion := (versionNumber allButFirst: lastPointIndex) asInteger + 1.
				(versionNumber  copyFrom: 1 to: lastPointIndex) , (newMinorVersion asString) ].
	^ self standardizeDevVersionString: newVersionNumber

]

{ #category : #private }
MTDevelopmentWorkfow >> notifyConfigurationModified [
	(MBConfigurationRoot current configurationInfoFor: project configurationClass class) 
		configurationClassModified
]

{ #category : #accessing }
MTDevelopmentWorkfow >> project [
	^ project
]

{ #category : #accessing }
MTDevelopmentWorkfow >> project: anObject [
	project := anObject
]

{ #category : #protocol }
MTDevelopmentWorkfow >> releaseDevelopment: versionName [
	"Release the development baseline (will give a version method) with the given version name."

	| devVersionString |
	devVersionString := self developmentVersion versionString.
	MetacelloToolBox
		createVersion: versionName
		for: project name
		from: devVersionString 
		description: 'version ' , versionName.
	self setStableBlessingFor: versionName.
	MetacelloToolBox 
		stableVersion: versionName 
		for: project name
		platformAttribute: #common. 
	"keep the development version pointing to the baseline"	
	self
		developmentVersion: devVersionString
		platformAttribute: #common.
	self notifyConfigurationModified
]

{ #category : #private }
MTDevelopmentWorkfow >> requiredProjectSpecs [
	^ project requiredProjects 
		collect: [ :requiredProject |  VersionnerToolBox projectSpecFromRequiredProject: requiredProject ].
]

{ #category : #private }
MTDevelopmentWorkfow >> setStableBlessingFor: versionName [
	(MetacelloToolBox configurationNamed: project name)
		modifyVersionMethodForVersion: versionName
			versionSpecsDo: [ :attr :versionSpec | 
					attr == #common
						ifTrue: [ versionSpec blessing: #stable ].
					true ];
		commitMethod
]

{ #category : #private }
MTDevelopmentWorkfow >> standardizeDevVersionString: aDevVersionString [
	| versionString |
	
	versionString := aDevVersionString copyReplaceAll: 'baseline-' with: ''.
	versionString  :=  versionString copyReplaceAll: '-baseline' with: ''.
	versionString  :=  versionString copyReplaceAll: 'baseline' with: ''.
	^ versionString , '-baseline'
]

{ #category : #protocol }
MTDevelopmentWorkfow >> updateDevelopment [
	"Update development baseline with information present in the project."

	| baseline versionString |
	versionString := self developmentVersion versionString.
	self isDevelopmentUsedInRelease 
		ifTrue: [ self createNextDevelopment: self nextDevelopmentVersionString ]
		ifFalse: [ baseline := MetacelloToolBox compiledMethodForVersion: self developmentVersion.
				baseline methodClass class removeSelector: baseline selector.
				self createBaseline: versionString ].
	self notifyConfigurationModified
]

{ #category : #private }
MTDevelopmentWorkfow >> version: versionString [
	^ project configurationClass project version: versionString
]
