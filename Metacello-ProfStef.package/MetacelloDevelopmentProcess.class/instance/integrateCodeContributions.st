lessons
integrateCodeContributions
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