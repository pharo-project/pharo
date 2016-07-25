private
createBaseline: baselineVersionString
	"Create a new baseline with information given by the project"
	MetacelloToolBox
		createBaseline: baselineVersionString
		for: project configurationClass name
		repository: project repository
		requiredProjects: (project requiredProjects collect: [ :each | each name ])
		packages: (project packages collect: [ :aPackage | aPackage name ])
		repositories: #()
		dependencies: project dependenciesMap
		groups: (project groups collect: [ :aGroup | aGroup name -> aGroup dependencies ])
		requiredProjectSpecs: self requiredProjectSpecs