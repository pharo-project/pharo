Class {
	#name : #MetacelloDevelopmentProcess,
	#superclass : #AbstractTutorial,
	#category : #Metacello-ProfStef
}

{ #category : #'tutorial metainfo' }
MetacelloDevelopmentProcess class >> title [	

	^'Metacello Development Cycle'
]

{ #category : #lessons }
MetacelloDevelopmentProcess >> checkpointSave [
	^Lesson
		title: '4. Checkpoint save'
		lesson: '"After having done development for awhile, you may want to checkpoint your work to share with others, test in another development environment or just to backup your work.

You can save the modified mcz files, update the development version spec with the new mcz file names and then save the configuration:"

	MetacelloToolBox 
		saveModifiedPackagesAndConfigurationIn: ConfigurationOfExample
		description: ''- fixed Issue 1090''.

"Or you can arrange to just save the modified mcz files and update the development version spec with the new mcz file names:"

	MetacelloToolBox 
		saveModifiedPackagesIn: ConfigurationOfExample
		description: ''- fixed Issue 1090''.

ProfStef next.
'
]

{ #category : #lessons }
MetacelloDevelopmentProcess >> compareVersions [
	^Lesson
		title: '6. Compare versions'
		lesson: '"Occasionally, it is useful to view the mcz and project version changes between the #development symbolic version and the #stable symbolic version:"

	(MetacelloToolBox compareVersionsIn: ConfigurationOfExample) inspect.
	
ProfStef next.
'
]

{ #category : #lessons }
MetacelloDevelopmentProcess >> createConfigurationAndBaseline [
	^Lesson
		title: '1. Create configuration'
		lesson: '"The MetacelloToolBox class provides programmatic support for creating and maintaining your configuration. 

Use the following expression to create your configuration class and initial baseline version. Edit the expression to specifiy the required projects, packages, dependencies and groups for your project:"
	
	MetacelloToolBox
		createBaseline: ''1.0-baseline''
		for: ''Example''
		repository: ''http://www.squeaksource.com/ProfStef''
		requiredProjects: #(''Shout'')
		packages: #(''ProfStef-Core'' ''ProfStef-Tests'')
		dependencies:
			{(''ProfStef-Core'' -> #(''Shout'')).
			(''ProfStef-Tests'' -> #(''ProfStef-Core''))}
		groups:
			{(''default'' -> #(''Core'')).
			(''Core'' -> #(''ProfStef-Core'')).
			(''Tests'' -> #(''ProfStef-Tests'')).
			(''Core Tests'' -> #(''Core'' ''Tests''))}.
			
"After evaluating the above expression, browse the configuration:"

	Smalltalk tools browser openOnClass: ConfigurationOfExample selector: #baseline10:.
	
"Edit the #baseline10 method in the browser to fine tune the baseline specification."

ProfStef next.
'
]

{ #category : #lessons }
MetacelloDevelopmentProcess >> createDevelopmentVersion [
	^Lesson
		title: '2. Create development version'
		lesson: '"After the configuration and initial baseline version have been created, define the initial development version:"
	
	MetacelloToolBox
		createDevelopment: ''1.0''
		for: ''Example''
		importFromBaseline: ''1.0-baseline''
		description: ''initial version''.

"After evaluating the above expression, take a look at the #version10: method in ConfigurationOfExample:"

	Smalltalk tools browser openOnClass: ConfigurationOfExample selector: #version10:.

"The current version of the Shout project has been filled in as well as the current version of the ProfStef packages.

When a new development version is created the #development symbolic version is defined as well:"

	Smalltalk tools browser openOnClass: ConfigurationOfExample selector: #development:.

"The #development version is defined as 1.0 for all platforms.

Note that the #development symbolic version is used in a number of the Development Support scripts, so you want to make sure that it is kept up-to-date."

ProfStef next.
'
]

{ #category : #lessons }
MetacelloDevelopmentProcess >> createNewDevelopmentVersion [
	^Lesson
		title: '8. Create new development version'
		lesson: '"After releasing a version it isn''t a bad idea to open a new development version for the next phase of development:"
	
	MetacelloToolBox
		createNewDevelopmentVersionIn: ConfigurationOfExample
		description: ''- aim at implementing new features''.

"The above expression uses the #release version as a template for the new version with the #release minor version number incremented." 

ProfStef next.
'
]

{ #category : #lessons }
MetacelloDevelopmentProcess >> integrateCodeContributions [
	^Lesson
		title: '5. Integrate code contributions'
		lesson: '"If other developers have commited new versions of the packages in your project you can integrate the changes into your configuration by first loading the latest packages in your project:"

	(ConfigurationOfExample project version: #baseline) load.

"Then updating the mcz file specifications in #development version:"

	MetacelloToolBox 
		updateToLatestPackageVersionsIn: ConfigurationOfExample
		description: ''- integrated code from Barney and Fred''.

"Then checkpoint the configuration:"

	MetacelloToolBox 
		saveConfigurationPackageFor: ''Example'' 
		description: ''- integrated code from Barney and Fred''.

ProfStef next.
'
]

{ #category : #intro }
MetacelloDevelopmentProcess >> introductionText [

	^'This tutorial covers some of the methods that can be found in the MetacelloToolBox.
CREATE PROJECT
	1. Create configuration and baseline version
	2. Create development version
	3. Validate and save configuration in project repository
	
DEVELOPMENT
	4. Checkpoint save
	5. Integrate code contributions
	6. Compare versions

RELEASE
	7. Release development version
	8. Create new development version

For convenience, a number of the methods covered in these sections can be found in the ''development support'' category on the class-side of MetacelloBaseConfiguration." 

	Smalltalk tools browser openOnClass: MetacelloBaseConfiguration class selector: #DevelopmentProcess.

"After deciding which of the development support methods you will use in your project, copy the methods of interest to the the class-side of your own configuration or implement your own ''development support'' methods using the MetacelloToolBox API:"

	HelpBrowser openOn: MetacelloAPIDocumentation
"'
]

{ #category : #lessons }
MetacelloDevelopmentProcess >> releaseVersion [
	^Lesson
		title: '7. Release development version'
		lesson: '"When you ready to release the #development version of your project, the following things need to be done:

	1. Change #blessing of version to #release
	2. Set the #development symbolic version to #notFound (no longer in development)
	3. Set the #stable symbolic version to the current #development version
	4. Save the configuration
	5. Copy the configuration to http://www.squeaksource.com/MetacelloRepository (optional).

Steps 1-4 are performed by the following expression:"

	MetacelloToolBox 
		releaseDevelopmentVersionIn: ConfigurationOfExample
		description: ''- release version 1.0''.
	
"If you want to copy the saved configuration to another repository, use the following expression:"

	MetacelloToolBox 
		copyConfiguration: ConfigurationOfExample 
		to: ''http://www.example.com/MetacelloRepository''.

ProfStef next.
'
]

{ #category : #tutorial }
MetacelloDevelopmentProcess >> tutorial [
	^#(  createConfigurationAndBaseline createDevelopmentVersion validateAndSaveConfiguration checkpointSave integrateCodeContributions compareVersions releaseVersion createNewDevelopmentVersion)
]

{ #category : #lessons }
MetacelloDevelopmentProcess >> validateAndSaveConfiguration [
	^Lesson
		title: '3. Validate and save configuration in project repository'
		lesson: '"Before saving a configuration into the project repository, the configuration should be validated and any Critical Issues should be addressed:"
		
	(MetacelloToolBox validateConfiguration: ConfigurationOfExample) explore.

"Use Gofer to save the configuration to your project repository:"

	Gofer new
		url: ''http://www.example.com/ExampleRepository'';
		package: ''ConfigurationOfExample'';
		commit: ''Initial configuration''.

"Once you''ve done your initial commit, you can use the following expression to save the configuration to your project repository:"

	MetacelloToolBox saveConfigurationPackageFor: ''Example'' description: ''commit message''.
	
"Note that MetacelloToolBox class>>saveConfigurationPackageFor:description: validates the configuration before saving."

ProfStef next.
'
]
