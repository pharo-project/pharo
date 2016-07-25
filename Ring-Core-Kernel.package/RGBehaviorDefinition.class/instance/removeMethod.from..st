private
removeMethod: aRGMethodDefinition from: aCollection
	"Removes aRGMethodDefinition from the collection received"

	aCollection removeKey: aRGMethodDefinition selector ifAbsent:[]