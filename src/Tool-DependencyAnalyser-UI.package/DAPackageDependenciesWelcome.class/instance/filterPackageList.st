initialization
filterPackageList
	| match itemsMatching |
	textPackageField text
		ifEmpty: [ self packageList items: self systemPackages ]
		ifNotEmpty: [ 
			match := '*' , textPackageField text , '*'.
			itemsMatching := self systemPackages select: [ :package | match match: package ].
			self packageList items: itemsMatching asSortedCollection ]