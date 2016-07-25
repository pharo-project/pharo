adding
addVersionInformationExtractedFrom: readableFileName to: versions
	| name |
	name := (readableFileName copyUpToLast: $.) copyUpTo: $(.
	name last isDigit
		ifTrue: [ 
			versions
				add:
					{(name copyUpToLast: $-).	"pkg name"
					((name copyAfterLast: $-) copyUpTo: $.).	"user"
					(((name copyAfterLast: $-) copyAfter: $.) asInteger ifNil: [ 0 ]).	"version"
					readableFileName }]