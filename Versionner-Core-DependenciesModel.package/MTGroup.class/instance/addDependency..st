protocol
addDependency: aDependency
	super addDependency: aDependency.
	self project ifNotNil: [ self project announcer announce: (MTProjectModelChanged project: self) ].