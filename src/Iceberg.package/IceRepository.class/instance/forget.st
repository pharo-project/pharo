repository handling
forget
	(self class unregisterRepository: self ifAbsent: [ nil ])
		ifNotNil: [ Iceberg announcer announce: (IceRepositoryForgotten repository: self) ]