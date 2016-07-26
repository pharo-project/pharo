instance creation
parseName: aString extension: extension
	"picked up from GoferVersionReference>>parseName:"

	| basicName package branch author versionNumber packageName |
	basicName := aString last isDigit
		ifTrue: [ aString ]
		ifFalse: [ (aString copyUpToLast: $.) copyUpTo: $( ].
	package := basicName copyUpToLast: $-.
	(package includes: $.)
		ifFalse: [ branch := '' ]
		ifTrue: [ 
			branch := '.' , (package copyAfter: $.).
			package := package copyUpTo: $. ].
	author := (basicName copyAfterLast: $-) copyUpToLast: $..
	versionNumber := (basicName copyAfterLast: $-) copyAfterLast: $..
	(versionNumber notEmpty and: [ versionNumber allSatisfy: [ :each | each isDigit ] ])
		ifTrue: [ versionNumber := versionNumber asNumber ]
		ifFalse: [ versionNumber := 0 ].
	packageName := package , branch.
	^ {packageName.
	author.
	versionNumber.
	basicName}