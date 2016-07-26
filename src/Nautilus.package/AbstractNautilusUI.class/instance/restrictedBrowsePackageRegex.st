menus behavior
restrictedBrowsePackageRegex
	| regex |
	
	regex := self selectedPackages
		ifEmpty: [ '' ]
		ifNotEmpty: [ :packages | 
			'^',(packages first name splitOn: $-) first , '-.*$'].
		
	regex := UIManager default request: 'Browse restricted on Packages matching:' regex: regex .
	regex ifNil: [ ^ self ].
	self restrictedBrowsePackages: (self browsedEnvironment packages select: [ :package| regex matches: package name ] )