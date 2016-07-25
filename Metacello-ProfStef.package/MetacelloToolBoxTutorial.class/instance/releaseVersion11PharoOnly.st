lessons
releaseVersion11PharoOnly

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
