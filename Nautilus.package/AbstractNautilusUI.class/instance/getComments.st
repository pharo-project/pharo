source code area
getComments

	^ self selectedClass
		ifNil: [
			self selectedPackage
				ifNil: [ '' ]
				ifNotNil: [:package | '' "package comment" ]]
		ifNotNil: [:class | class comment ]