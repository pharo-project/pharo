analyzing
clusterReferencesDo: aBlock
	"Ensures that nil could be encoded, later in references step."

	super clusterReferencesDo: aBlock.
	aBlock value: nil