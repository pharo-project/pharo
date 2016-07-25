query
allThemeClasses
	"Answer the subclasses of the receiver that are considered to be
	concrete (useable as a theme)."

	^(self allSubclasses reject: [:c | c isAbstract]) asSortedCollection: [:a :b |
		a themeName <= b themeName]