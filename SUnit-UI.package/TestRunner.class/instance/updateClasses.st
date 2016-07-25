updating
updateClasses
	| classesForCategories |
	classesForCategories := self findClassesForPackages: packagesSelected.
	
	classPattern ifNotNil: [
		classesForCategories := classesForCategories reject: [:cls| (classPattern matchesIn: cls name) isEmpty ]].
	
	classes := classesForCategories asArray
		sort: [ :a :b | self sortClass: a before: b ].
	classIndex := 0.
	classesSelected := classesSelected isNil
		ifTrue: [ classesForCategories ]
		ifFalse: [ 
			classesSelected
				select: [ :each | classesForCategories includes: each ] ].
	self changed: #classList; 
		changed: #classSelected; 
		changed: #hasRunnable.