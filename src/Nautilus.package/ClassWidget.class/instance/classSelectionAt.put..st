private list model
classSelectionAt: anIndex put: aBoolean
	| elt |
	aBoolean ifNil: [ ^ self ].
	elt := self getClassesList at: anIndex ifAbsent: [ ^ self ].
	self classesSelection at: elt put: aBoolean.
	self model changed: #hasSelectedSelections