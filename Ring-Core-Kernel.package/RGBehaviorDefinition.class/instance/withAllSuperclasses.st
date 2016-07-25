accessing class hierarchy
withAllSuperclasses
	"if allSuperclasses is stored should not affect the collection"

	^self allSuperclasses, {self}