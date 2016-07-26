api
project: anMTProject
	project := anMTProject.
	packagesList roots: self packages.
	project ifNotNil: 
		[ project announcer weak 
			when: MTProjectModelChanged
			send: #refreshPackages
			to: self ].