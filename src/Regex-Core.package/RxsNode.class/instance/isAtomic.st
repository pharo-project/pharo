testing
isAtomic
	"Answer whether the node is atomic, i.e. matches exactly one 
	constant predefined normal character.  A matcher may decide to 
	optimize matching of a sequence of atomic nodes by glueing them 
	together in a string."

	^false "tentatively"