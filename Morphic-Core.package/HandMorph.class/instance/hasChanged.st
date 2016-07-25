drawing
hasChanged
	"Return true if this hand has changed, either because it has moved or because some morph it is holding has changed."

	^ hasChanged ifNil: [ true ]
