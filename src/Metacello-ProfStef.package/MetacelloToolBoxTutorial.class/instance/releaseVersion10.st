lessons
releaseVersion10

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
