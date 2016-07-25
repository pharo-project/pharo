running
execute
	self model hasVersions
		ifTrue: [ self model load ].
	self gofer cleanup