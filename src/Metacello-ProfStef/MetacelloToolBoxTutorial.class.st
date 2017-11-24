Class {
	#name : #MetacelloToolBoxTutorial,
	#superclass : #AbstractTutorial,
	#category : #'Metacello-ProfStef'
}

{ #category : #'tutorial metainfo' }
MetacelloToolBoxTutorial class >> title [	

	^'Inside Metacello Toolbox API'
]

{ #category : #lessons }
MetacelloToolBoxTutorial >> create10Baseline [

	^Lesson
		title: 'Create 1.0 Baseline'
		lesson: 
'"
	1. Create configuration
		- class named ConfigurationOfExample created if it does not already exist.
	2. Prepare to define baseline version method
		- selector, category and version number defined
	3. Define baseline project structure
		- repository
		- external project ''Shout''
		- packages ''ProfStef-Core'' and ''ProfStef-Tests'', plus dependencies
	4. Compile the #baseline10: method
		- version method compiled and validated "
	
"1."	(MetacelloToolBox configurationNamed: ''Example'')
"2."		createVersionMethod: ''baseline10:'' inCategory: ''baselines'' forVersion: ''1.0-baseline'';
"3."		addSection: #common
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
				(''Core Tests'' -> #(''Core'' ''Tests''))}
			versionSpecsDo: [ :versionSpec | versionSpec blessing: #baseline ];
"4."		commitMethod.

"After evaluating the above expression, browse the configuration:"

	ConfigurationOfExample browse.
	
"and look at the generated method #baseline10:. Note that the symbolic version #bleedingEdge is used for the Shout project."

ProfStef next.
'

]

{ #category : #lessons }
MetacelloToolBoxTutorial >> create11Baseline [

	^Lesson
		title: 'Create 1.1 Baseline'
		lesson: 
'"
	1. Create configuration
	2. Prepare to define baseline version method
	3. Copy specs from 1.0-baseline. This is a copy not an import, so all specs are preserved
		- with #updateProjects set to false, the version of the project Shout is preserved
		- with #updatePackages set to false, the specification of the mcz files for the packages ProfStef-Core ProfStef-Tests are preserved (typically not specified)
	4. Add and additional section for the attribute #pharo
		- the package ProfStefBrowser and it''s dependencies are added
		- the package is added to the UI group
	5. Compile and validate #baseline11: method
	6. Prepare to modify the method defining version 1.1.
		- false as return value in block means that all existing version specs in 1.1 are dropped on floor. Basically just the method name and pragma are preserved
	7. Import the versionSpecs from 1.1-baseline into version 1.1
	8. Compile and validate the #version11: method
	9. Prepare to modify the method defining symbolic version #development. Preserve existing symbolic version specs
	10.  add symbolic version spec for #pharo
	11. Compile and validate the #development: method
"
	
	| description |
	description := ''''.
"1."	(MetacelloToolBox configurationNamed: ''Example'')
"2"		createVersionMethod: ''baseline11:'' inCategory: ''baselines'' forVersion: ''1.1-baseline'';
"3."		addSectionsFrom: ''1.0-baseline''
			forBaseline: true
			updateProjects: false
			updatePackages: false
			versionSpecsDo: [ :attribute :versionSpec | true ];
"4"		addSection: #pharo
			repository: nil
			requiredProjects: #()
			packages: #(''ProfStefBrowser'')
			dependencies: {(''ProfStefBrowser'' -> #(''ProfStef-Core''))}
			groups: {(''UI'' -> #(''ProfStefBrowser''))}
			versionSpecsDo: [ :versionSpec |  ];
"5."		commitMethod;
"6."		modifyVersionMethodForVersion: ''1.1'' versionSpecsDo: [ :attribute :versionSpec | false ];
"7."		importFrom: ''1.1-baseline''
			updateProjects: true
			updatePackages: true
			versionSpecsDo: [ :attribute :versionSpec | 
				attribute == #common
					ifTrue: [ 
						"update the author, blessing and timeStamp"
						versionSpec
							author: MetacelloPlatform current authorName;
							blessing: #development;
							description: description;
							timestamp: MetacelloPlatform current timestamp ].
				true ];
"8."		commitMethod;
"9"		modifySymbolicVersionMethodFor: #development symbolicVersionSpecsDo: [ :symbolicVersionSpec | true ];
"10."	addSymbolicSection: #pharo version: ''1.1'';
"11."	commitMethod.

"Look at the generated methods: 

	#baseline11:
	#version11:
	#development:"
	
ProfStef next.
'

]

{ #category : #lessons }
MetacelloToolBoxTutorial >> intro [

	^Lesson
		title: 'Intro'
		lesson: 
'"The lessons in this section cover some of the same material that was covered in the development process tutorial, but we take a closer look at the implementation using the instance-side protocol instead of the class-side protocol"
	
ProfStef next.
'

]

{ #category : #intro }
MetacelloToolBoxTutorial >> introductionText [

	^'Dive down into the MetacelloToolBox API to take a closer look at the implementation of the development support methods.'
]

{ #category : #lessons }
MetacelloToolBoxTutorial >> open10ForDevelopment [

	^Lesson
		title: 'Open version 1.0 for development'
		lesson: 
'"
	1. Create configuration
		- always start a toolbox expression this way
	2. Prepare to define version method #version10:
		- similar expression used to create baseline method
	3. Import specification from version 1.0-baseline
		- specs that are not needed in a version are dropped
		- import is added to pragma
		- with #updateProjects set to true, the current version of the project Shout will set
		- with #updatePackages set to true, the current mcz files for the packages ProfStef-Core ProfStef-Tests will be set
	4. Define desired versionSpec attributes
		- current author and timestamp
		- set blessing to #development
		- set the description
		- return value of true in block, means the versionSpec is added to current method being defined
	5. Compile the #version10: method
		- compile and validate
	6. Prepare to define symbolic version method #development:
	7. Define symbolic version 
		- symbolic version #development maps to version 1.0 for the #common attributes
		- multiple addSymbolicSection: expressions may be used 
	8. Compile the #development: method
		- compile and validate"
	
	| description |
	description := ''- fixed a bug''.
"1."	(MetacelloToolBox configurationNamed: ''Example'')
"2."		createVersionMethod: ''version10:'' inCategory: ''versions'' forVersion: ''1.0'';
"3."		importFrom: ''1.0-baseline''
			updateProjects: true
			updatePackages: true
			versionSpecsDo: [ :attribute :versionSpec | 
				attribute == #common
					ifTrue: [ 
"4."						versionSpec
							author: MetacelloPlatform current authorName;
							blessing: #development;
							description: description;
							timestamp: MetacelloPlatform current timestamp ].
				true ];
"5."		commitMethod;
"6."		createSymbolicVersionMethod: ''development:'' inCategory: ''symbolic versions'' forVersion: #development;
"7."		addSymbolicSection: #common version: ''1.0'';
"8."		commitMethod.

"After evaluating the above expression, take a look at the generated methods:
	- #version10:
	- #development:"

ProfStef next.
'

]

{ #category : #lessons }
MetacelloToolBoxTutorial >> open11ForDevelopment [

	^Lesson
		title: 'Open version 1.1 for development'
		lesson: 
'"
	1. Create configuration
	2. Prepare to define version method #version11:
	3. Use version 1.0 as basis for version 1.1. basically a copy. Differs from import.
	4. Update project and package versions
		- with #updateProjects set to true, the current version of the project Shout will set
		- with #updatePackages set to true, the current mcz files for the packages ProfStef-Core ProfStef-Tests will be set
	5. Define desired versionSpec attributes
		- current author and timestamp
		- set blessing to #development
		- set the description
	6. Compile and validate the #version11: method
	7. Prepare to modify symbolic version method #development:
		- keep all of the existing symbolic version specs in the method
	8. Define/change the symbolic version for #development to 1.1
	9. Compile and validate the #development: method"
	
	| description |
	description := ''''.
"1."	(MetacelloToolBox configurationNamed: ''Example'')
"2."		createVersionMethod: ''version11:'' inCategory: ''versions'' forVersion: ''1.1'';
"3."		addSectionsFrom: ''1.0''
			forBaseline: false
"4."			updateProjects: true
			updatePackages: true
			versionSpecsDo: [ :attribute :versionSpec | 
				attribute == #common
					ifTrue: [ 
"5."						versionSpec
							author: MetacelloPlatform current authorName;
							description: description;
							blessing: #development;
							timestamp: MetacelloPlatform current timestamp ].
				true ];
"6."		commitMethod;
"7."		modifySymbolicVersionMethodFor: #development symbolicVersionSpecsDo: [ :symbolicVersionSpec | true ];
"8."		addSymbolicSection: #common version: ''1.1'';
"9."		commitMethod.

"After evaluating the above expression, take a look at the generated methods:
	- #version11:
	- #development:"


ProfStef next.
'

]

{ #category : #lessons }
MetacelloToolBoxTutorial >> releaseVersion10 [

	^Lesson
		title: 'Release version 1.0'
		lesson: 
'"
	1. Create configuration
	2. Prepare to modify the version method for version 1.0:
		- the project and package versions are not updated
	3. Change the blessing to #release for the #common attribute
	4. Note that the return value for the #attributeBlocksDo: block is true. If the return value is false, the versionSpec would not be included in the updated mehtod.
	5. Compile and validate the #version10: method .
	6. Remove version 1.0 from the definition of the #development symbolic version - the version is no longer in development
		- the return value for the  #methodSectionsDo: block (like the #versionSpecsDo: block) determins whether the methodSection is carried forward or not
	7. Compile and validate the #development: method
	8. Prepare to define the symbolic version method #stable:
	9. Define version 1.0 as the #stable symbolic version for #common
	10. Compile and validate the #stable: method"
	
"1"	(MetacelloToolBox configurationNamed: ''Example'')
"2."		modifyVersionMethodForVersion: ''1.0''
			versionSpecsDo: [ :attribute :versionSpec | 
					attribute == #common
"3."						ifTrue: [ versionSpec blessing: #release ].
"4."					true ];
"5."		commitMethod;
"6."		modifySymbolicVersionMethodFor: #development
			symbolicVersionSpecsDo: [ :methodSection | methodSection versionString ~= ''1.0'' ];
"7."		commitMethod;
"8."		createSymbolicVersionMethod: ''stable:'' inCategory: ''symbolic versions'' forVersion: #stable;
"9."		addSymbolicSection: #common version: ''1.0'';
"10."	commitMethod.

"After evaluating the above expression, take a look at the updated method #version10, where you will see that the blessing has been updated to #release.

Look at the #development: method (which should be empty) and the #stable: method"

ProfStef next.
'

]

{ #category : #lessons }
MetacelloToolBoxTutorial >> releaseVersion11PharoOnly [

	^Lesson
		title: 'Release version 1.1 (#pharo only)'
		lesson: 
'"
	1. Create configuration
	2. Prepare to modify the version method for version 1.1:
	3. Change the blessing to #release for the #pharo attribute
	4. Compile and validate the #version11: method .
	5. Remove version 1.0 from the definition of the #development symbolic version
	6. Compile and validate the #development: method
	7. Prepare to define the symbolic version method #stable:
	8. Define version 1.1 as the #stable symbolic version for #common
	9. Compile and validate the #stable: method"
	
"1."	(MetacelloToolBox configurationNamed: ''Example'')
"2."		modifyVersionMethodForVersion: ''1.1''
			versionSpecsDo: [ :attribute :versionSpec | 
					attribute == #pharo
"3."						ifTrue: [ versionSpec blessing: #release ].
					true ];
"4."		commitMethod;
"5."		modifySymbolicVersionMethodFor: #development
			symbolicVersionSpecsDo: [ :symbolicVersionSpec | symbolicVersionSpec attribute ~~ #pharo ];
"6."		commitMethod;
"7."		modifySymbolicVersionMethodFor: #stable symbolicVersionSpecsDo: [ :symbolicVersionSpec | true ];
"8."		addSymbolicSection: #pharo version: ''1.1'';
"9."		commitMethod.

"After evaluating the above expression, take a look at the generated methods:

	#version11:
	#development:
	#stable: "
	
ProfStef next.
'

]

{ #category : #tutorial }
MetacelloToolBoxTutorial >> tutorial [

	^#(
		intro
		create10Baseline
		open10ForDevelopment 
		updateVersion10
		releaseVersion10
		open11ForDevelopment
		create11Baseline
		releaseVersion11PharoOnly
		)
]

{ #category : #lessons }
MetacelloToolBoxTutorial >> updateVersion10 [

	^Lesson
		title: 'Update specs for version 1.0'
		lesson: 
'"
	1. Create configuration
	2. Prepare to update the version method for version 1.0:
		- with #updateProjects set to true, the current version of the project Shout will set
		- with #updatePackages set to true, the current mcz files for the packages ProfStef-Core ProfStef-Tests will be set
	3. Define desired versionSpec attributes
		- current author and timestamp
	4. Append the #logEntry to the version descripttion
	5. Compile the #version10: method
		- compile and validate"
	
	| logEntry |
	logEntry := ''- fixed a bug''.
"1."	(MetacelloToolBox configurationNamed: ''Example'')
"2."		updateVersionMethodForVersion: ''1.0''
			versionSpecsDo: [ :attribute :versionSpec | 
				attribute == #common
					ifTrue: [ 
"3."						versionSpec
							author: MetacelloPlatform current authorName;
							timestamp: MetacelloPlatform current timestamp.
"4."						versionSpec description value isEmpty
							ifTrue: [ versionSpec description: logEntry ]
							ifFalse: [ 
								| strm |
								strm := WriteStream on: String new.
								strm
									nextPutAll: versionSpec description value;
									cr;
									nextPutAll: logEntry.
								versionSpec description: strm contents ] ].
				true ];
"5."		commitMethod.

"After evaluating the above expression, take a look at the updated method #version10. Unless you changed the packages that were loaded in your image, you should just see the #timestamp and #description fields updated."

ProfStef next.
'

]
