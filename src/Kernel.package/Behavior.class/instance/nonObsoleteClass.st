initialize-release
nonObsoleteClass
	"Attempt to find and return the current version of this obsolete class"

	| obsName |
	obsName := self name.
	[obsName beginsWith: 'AnObsolete']
		whileTrue: [obsName := obsName copyFrom: 'AnObsolete' size + 1 to: obsName size].
	^ self environment at: obsName asSymbol