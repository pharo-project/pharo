private list model
classSelectionAt: anIndex
	
	| elt |
	elt := self getClassesList at: anIndex ifAbsent: [ ^false ].
	^ self classesSelection at: elt ifAbsent: [ false ]