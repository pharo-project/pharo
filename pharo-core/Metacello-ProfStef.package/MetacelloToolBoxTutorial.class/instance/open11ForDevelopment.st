lessons
open11ForDevelopment

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
