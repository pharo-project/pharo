initialization
parseName: aString
	| basicName |
	basicName := aString last isDigit
		ifTrue: [ aString ]
		ifFalse: [ (aString copyUpToLast: $.) copyUpTo: $( ].
	package := basicName copyUpToLast: $-.
	(package includes: $.)
		ifFalse: [ branch := '' ]
		ifTrue: [
			branch := package copyAfter: $..
			package := package copyUpTo: $. ].
	author := (basicName copyAfterLast: $-) copyUpToLast: $..
	versionNumber := (basicName copyAfterLast: $-) copyAfterLast: $..
	(versionNumber notEmpty and: [ versionNumber allSatisfy: [ :each | each isDigit ] ])
		ifTrue: [ versionNumber := versionNumber asInteger ]
		ifFalse: [ versionNumber := 0 ]