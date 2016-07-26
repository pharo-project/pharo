lessons
open10ForDevelopment

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
