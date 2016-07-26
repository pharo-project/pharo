class and trait names
classOrTraitNamed: aString
	"aString is either a class or trait name or a class or trait name followed by ' class' or 'classTrait' respectively. Answer the class or metaclass it names."

	^ globals classOrTraitNamed: aString