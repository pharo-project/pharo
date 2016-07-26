accessing-packages
packageAt: anIndex
	^ packagesSelected includes: (packages at: anIndex ifAbsent: [ ^ false ]).