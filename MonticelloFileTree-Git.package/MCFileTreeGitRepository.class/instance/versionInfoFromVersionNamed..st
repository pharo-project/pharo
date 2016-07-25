accessing
versionInfoFromVersionNamed: aString
	"Ok this code hides some of the conflict/version upgrade resolution inside the repository. This is plain bad design from Monticello/Metacello."

	| versions |
	versions := self readableFileNames select: [ :each | each name beginsWith: aString ].
	versions isEmpty
		ifTrue: [ ^ nil ].
	versions := versions
		asSortedCollection: [ :a :b | (a name copyAfterLast: $.) asNumber <= (b name copyAfterLast: $.) asNumber ].
	^ versions last info