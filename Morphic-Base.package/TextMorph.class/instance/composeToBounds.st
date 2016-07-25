private
composeToBounds
	"Compose my text to fit my bounds.
	If any text lies outside my bounds, it will be clipped, or
	if I have successors, it will be shown in the successors."
	| |
	self releaseParagraph; paragraph.
	container ifNotNil:
		[self privateBounds: container bounds truncated].
	self paragraph positionWhenComposed: self position.
	successor ifNotNil:
		[successor predecessorChanged].

