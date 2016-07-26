reflectivity
visitStorePopIntoTempNode: aNode
	"we store the TOS into the temp defined by the reification node"

	aNode binding emitStore: methodBuilder.
	methodBuilder popTop.