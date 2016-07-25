A project represents a software development project, and more presicely, its depdendencies (also known as configuration).

Note: Need to add extra methods with "for:" parameter to handle platforms (e.g. for: #'pharo' do)

Instance Variables
	- configurationClass : the configurationClass this project represents
	- repository : The core repository URL of this project
	- packages : internal packages (MTPackage) the project depends on
	- groups : definitions of set of dependencies (MTGroups)
	- depedentProjects : list of external projects this project depends on (list of project names)
