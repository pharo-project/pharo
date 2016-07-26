private
apply: aBlock
	"apply the given block to the inner canvas with clipRect as the clipping rectangle"
	canvas clipBy: clipRect during: aBlock