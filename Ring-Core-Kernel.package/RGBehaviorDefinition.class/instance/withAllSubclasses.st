accessing class hierarchy
withAllSubclasses
	"if allSubclasses is stored should not affect the collection"

	^self allSubclasses, {self}