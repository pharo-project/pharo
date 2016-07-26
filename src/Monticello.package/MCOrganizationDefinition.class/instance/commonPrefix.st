accessing
commonPrefix
	"Answer the package common name. 
	 In ancient times, this was answering X-Y-, with a minus at the end... this was useful for the 
	 old PackageInfo framework. RPackage does not do pattern matching, thus this is not needed anymore 
	 (and provoques an error)"
	| prefix |
	prefix := self basicCommonPrefix.
	^(prefix notEmpty and: [ prefix endsWith: '-' ])
		ifTrue: [ prefix allButLast ]
		ifFalse: [ prefix ].
	
