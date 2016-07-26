accessing
project
	"NOTE: The MetacelloNullRecordingMCSpecLoader is being used to prevent packages
		from being loaded, see MetacelloConfigTemplate>>project for an example #project
		method that loads the package for real"

	^ project ifNil: [ | constructor loader |
		"Construct Metacello project"
		constructor := (Smalltalk at: #MetacelloVersionConstructor) on: self.
		project := constructor project.
		loader := MetacelloNullRecordingMCSpecLoader new.
		loader evalDoits: true.
		project loader: loader.
		project ]