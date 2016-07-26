lessons
updateVersion10

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
