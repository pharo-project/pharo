lessons
releaseVersion
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