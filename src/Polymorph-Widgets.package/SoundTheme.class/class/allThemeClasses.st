accessing
allThemeClasses
	"Answer the subclasses of the receiver that are considered to be
	concrete (useable as a theme)."

	^self withAllSubclasses reject: [:c | c isAbstract]