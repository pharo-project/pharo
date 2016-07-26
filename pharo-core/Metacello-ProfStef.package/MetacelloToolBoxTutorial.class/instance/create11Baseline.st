lessons
create11Baseline

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
