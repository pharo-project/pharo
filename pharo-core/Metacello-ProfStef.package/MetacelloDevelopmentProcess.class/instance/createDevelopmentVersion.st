lessons
createDevelopmentVersion
	^Lesson
		title: '2. Create development version'
		lesson: '"After the configuration and initial baseline version have been created, define the initial development version:"
	
	MetacelloToolBox
		createDevelopment: ''1.0''
		for: ''Example''
		importFromBaseline: ''1.0-baseline''
		description: ''initial version''.

"After evaluating the above expression, take a look at the #version10: method in ConfigurationOfExample:"

	Smalltalk tools browser fullOnClass: ConfigurationOfExample selector: #version10:.

"The current version of the Shout project has been filled in as well as the current version of the ProfStef packages.

When a new development version is created the #development symbolic version is defined as well:"

	Smalltalk tools browser fullOnClass: ConfigurationOfExample selector: #development:.

"The #development version is defined as 1.0 for all platforms.

Note that the #development symbolic version is used in a number of the Development Support scripts, so you want to make sure that it is kept up-to-date."

ProfStef next.
'