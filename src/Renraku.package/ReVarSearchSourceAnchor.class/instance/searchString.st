as yet unclassified
searchString
	^ sourceEntity instVarNames last = super searchString
		ifTrue:  [ super searchString, '''' ]
		ifFalse:	[ super searchString, ' ' ]