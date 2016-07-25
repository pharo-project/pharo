private
collectCommitActions
	| developmentVersion project collector |
	
	(VSCommitProjectValidator basename: (self configurationClass name configurationBaseName)) validate.
	developmentVersion := self configurationClass project version: #development. 
	project := MTProject
		newFromVersion: developmentVersion
		inConfiguration: self configurationClass.

	collector := VSDirtyCollector strategy: self commitStrategy.
	project acceptVisitor: collector.

	projectActions := collector projects collect: #asCommitAction as: Array.
	"Force main project at least"
	(projectActions isEmpty and: [ self commitStrategy shouldCommitProjects ]) 
		ifTrue: [ projectActions := { project asCommitAction } ].
	packageActions := collector packages collect: #asCommitAction as: Array