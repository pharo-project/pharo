actions
exportToStFiles: aDirectoryName subgroup: subgroup
	
	self exportToStFiles: aDirectoryName initializers: true group: 'full-', subgroup.
	self exportToStFiles: aDirectoryName initializers: false group: 'full-', subgroup.