source code area
getComments
	^ self selectedClass
		ifNil: [ self selectedPackage
				ifNil: [ '' ]
				ifNotNil: [ :package | 
					package isGroup
						ifTrue: [ '' ]
						ifFalse: [ package packageComment ] ] ]
		ifNotNil: [ :class | class comment ]