visitor-double dispatching
visitArrayNode: anArrayNode 
	"An arrayNode may have side effect, for example: '{self foo}'. So we cannot just generate nothing for this node."

	super visitArrayNode: anArrayNode.
	methodBuilder popTop.
