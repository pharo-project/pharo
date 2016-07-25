visitor
visitCategoryChange: aChange

	| package |
	package := RPackageOrganizer default packageNamed: aChange affectedPackageName.
	package classTags
		detect: [ :tag |
			tag categoryName = aChange categoryName
		]
		ifFound: [ :tag | Smalltalk tools browser openOnPackage: tag ]
		ifNone: [ self inform: 'Category ' , aChange categoryName , ' not found.' ].