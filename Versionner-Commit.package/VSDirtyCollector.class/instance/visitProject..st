visitor
visitProject: aMTProject
	| oldProjects oldPackages |
	
	oldProjects := projects.
	oldPackages := packages. 
	projects := OrderedCollection new.
	packages := OrderedCollection new.

	aMTProject requiredProjects do: [ :each | each acceptVisitor: self ].
	aMTProject packages do: [ :each | each acceptVisitor: self ].
	
	(self commitStrategy shouldCommitProjects
		and: [ 
			projects notEmpty 
				or: [ packages notEmpty 
				or: [ self commitStrategy shouldCommitNestedProjects ] ] ])
		ifTrue: [ projects add: aMTProject ].

	projects := 
		oldProjects, 
		(projects select: [ :eachProject | 
			oldProjects noneSatisfy: [ :each | each name = eachProject name  ] ]).
	packages := 
		oldPackages, 
		(packages select: [ :eachPackage | 
			oldPackages noneSatisfy: [ :each | each name = eachPackage name  ] ])