testing
includesKey: aKey
	"Test if the property aKey or pragma with selector aKey is present."

	1 to: self basicSize do:
		[:i |
		(self basicAt: i) key == aKey ifTrue:
			[^true]].
	^false