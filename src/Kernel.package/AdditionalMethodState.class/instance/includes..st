testing
includes: aPropertyOrPragma "<Association|Pragma>"
	"Test if the property or pragma is present."

	1 to: self basicSize do:
		[:i |
		(self basicAt: i) = aPropertyOrPragma ifTrue:
			[^true]].
	^false