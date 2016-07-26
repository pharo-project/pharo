lessons
checkpointSave
	^Lesson
		title: '4. Checkpoint save'
		lesson: '"After having done development for awhile, you may want to checkpoint your work to share with others, test in another development environment or just to backup your work.

You can save the modified mcz files, update the development version spec with the new mcz file names and then save the configuration:"

	MetacelloToolBox 
		saveModifiedPackagesAndConfigurationIn: ConfigurationOfExample
		description: ''- fixed Issue 1090''.

"Or you can arrange to just save the modified mcz files and update the development version spec with the new mcz file names:"

	MetacelloToolBox 
		saveModifiedPackagesIn: ConfigurationOfExample
		description: ''- fixed Issue 1090''.

ProfStef next.
'