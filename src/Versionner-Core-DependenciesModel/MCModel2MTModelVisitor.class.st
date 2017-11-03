"
A MCModel2MTModelVisitor visits a Metacello model to transform it to a MT Model .

Instance variables:
	- project : the root element (an MTProject) of the target model
	
	
MCModel2MTModelVisitor new 
	visitConfiguration: ConfigurationOfVersionner  
	withVersion: (ConfigurationOfVersionner project version: #development)
"
Class {
	#name : #MCModel2MTModelVisitor,
	#superclass : #Object,
	#instVars : [
		'project'
	],
	#category : #Versionner-Core-DependenciesModel
}

{ #category : #visiting }
MCModel2MTModelVisitor >> visitConfiguration: aConfigurationClass [
	"Visits a Configuration class with the current version."

	| currentVersion |
	
	currentVersion := aConfigurationClass project currentVersion.
	^ self visitConfiguration: aConfigurationClass withVersion: currentVersion.

]

{ #category : #visiting }
MCModel2MTModelVisitor >> visitConfiguration: aConfigurationClass withVersion: aVersion [
	"visits a Configuration class for the specified version and returns the project root object.
	 Returns the project root object or nil if the given version is nil."

	aVersion ifNil: [ ^ nil ].
	project := MTProject  new.
	project configurationClass: aConfigurationClass.
	self visitMCProject: aConfigurationClass project withVersion: aVersion.
	^project
	
]

{ #category : #visiting }
MCModel2MTModelVisitor >> visitConfiguration: aConfigurationClass withVersionString: aVersionString [
	"visits a Configuration class for the specified version and returns the project root object.
	 Returns the project root object or nil if the given version is nil."

	project := MTProject  new.
	project configurationClass: aConfigurationClass.
	self visitMCProject: aConfigurationClass project withVersion: (aConfigurationClass project version: aVersionString).
	^project
	
]

{ #category : #visiting }
MCModel2MTModelVisitor >> visitMCGroup: aGroup [
	"visit a Metacello Group"	
	| group |

	group := MTGroup new.
	group name: aGroup name.
	aGroup includes do: 
		[ :dep | 
		group addDependency: dep].
	project addDependency: group.
	^ group.
	
]

{ #category : #visiting }
MCModel2MTModelVisitor >> visitMCPackage: aPackage [
	"visit a Metacello Package"

	| package version |
	
	package := MTPackage new.
	package name: aPackage name.
	
	version := MTVersion fromVersionName: aPackage file.
	package version: version.
	
	package dependencies: aPackage requires asOrderedCollection.
	
	project addDependency: package.
	^ package.

]

{ #category : #visiting }
MCModel2MTModelVisitor >> visitMCProject: aProject withVersion: aVersion [
	"visit a Metacello Project with the specified version."
	| version |
	
	version := MTVersion fromVersionString: aVersion versionString.
	project version: version.
	aVersion ifNotNil: [ self visitMCVersion: aVersion ].
]

{ #category : #visiting }
MCModel2MTModelVisitor >> visitMCProjectAsRequiredProject: aProject [
	"visit a Metacello Project to add it as a required projet. It is quite the same as visitMCProject except that we won't dive into the project structure.
	aProject is an instance of MetacelloMCProjectSpec."
	| version requiredProject |
	
	requiredProject := MTDependantProject new.
	requiredProject displayName: aProject name.
	requiredProject name: (aProject className ifNil: [aProject name] ifNotNil: [ :className | className configurationBaseName]).
	requiredProject configurationClass: aProject projectClass.
	requiredProject repositories: (aProject repositorySpecs collect: [ :aRepoSpec | aRepoSpec description]).
	requiredProject dependencies: (aProject loads ifNil: [ #() ] ifNotNil: [ :loads | loads ]) asOrderedCollection.
	
	version := MTVersion fromVersionString: aProject versionString.
	requiredProject version: version.
	
	project addDependency: requiredProject.
	^ requiredProject.
]

{ #category : #visiting }
MCModel2MTModelVisitor >> visitMCVersion: aVersion [
	"visit a Metacello Version"
	
	| groups packages projects repositories |
	
	project version
		description: aVersion spec description value;
		author: aVersion spec author value;
		timestamp: aVersion spec timestamp value.

	groups := aVersion groups.
	groups do:  [ :aGroup | self visitMCGroup: aGroup ].
	
	packages := aVersion packages.
	packages do:  [ :aPackage | self visitMCPackage: aPackage ].
	
	projects := aVersion projects.
	projects do:  [ :aProject | self visitMCProjectAsRequiredProject: aProject ].
	
	repositories := aVersion spec repositories list.
	repositories ifNotEmpty:  
		[:repo | project repository: repo first name].

]
