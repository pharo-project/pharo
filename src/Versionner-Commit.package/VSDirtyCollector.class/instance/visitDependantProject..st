visitor
visitDependantProject: aMTDependantProject 
	"Dependent projects are handled just as regular projects"
	| class version |
	
	self commitStrategy shouldNestProjects ifFalse: [ ^ self ].
	aMTDependantProject isPresent ifFalse: [ ^ self ].
	(self projectVisited: aMTDependantProject) ifTrue: [ ^ self ].
	
	class := aMTDependantProject configurationClass.
	version := class project version: aMTDependantProject version versionString.
	self visitProject: (MTProject 
		newFromVersion: version 
		inConfiguration: class)
	