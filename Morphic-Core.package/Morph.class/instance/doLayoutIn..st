layout
doLayoutIn: layoutBounds 
	"Compute a new layout based on the given layout bounds."

	"Note: Testing for #bounds or #layoutBounds would be sufficient to
	figure out if we need an invalidation afterwards but #outerBounds
	is what we need for all leaf nodes so we use that."

	| box priorBounds |
	priorBounds := self outerBounds.
	submorphs isEmpty ifTrue: [^fullBounds := priorBounds].
	"Send #ownerChanged to our children"
	submorphs do: [:m | m ownerChanged].
	self layoutPolicy ifNotNil: [:layout | layout layout: self in: layoutBounds].
	self adjustLayoutBounds.
	fullBounds := self privateFullBounds.
	box := self outerBounds.
	box = priorBounds 
		ifFalse: [self invalidRect: (priorBounds quickMerge: box)]