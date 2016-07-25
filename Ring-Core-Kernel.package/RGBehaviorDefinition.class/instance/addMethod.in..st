private
addMethod: aRGMethodDefinition in: aCollection
	"Adds aRGMethodDefinition in the collection received"

	aRGMethodDefinition parent ifNil:[ aRGMethodDefinition parent: self ].
	aCollection at: aRGMethodDefinition selector
		put: aRGMethodDefinition