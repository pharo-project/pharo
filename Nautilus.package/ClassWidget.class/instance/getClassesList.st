protocol
getClassesList

	^ classesListElements ifNil: [ classesListElements := self loadClassesListElements ].