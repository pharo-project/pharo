recategorized
layoutChanged
	"Fixed to always flush layout cache - finally tracked down
	layout anomalies due to cached extents in layout
	policies not being flushed, the previous (incorrect) assumption being
	that it did not matter if layout was to be recomputed (fullBounds being nil).
	Recomputing of the layout apparently does not flush so must be done here."
	
	| layout |
	fullBounds := nil.
	layout := self layoutPolicy.
	layout ifNotNil:[layout flushLayoutCache].
	owner ifNotNil: [owner layoutChanged].
	"note: does not send #ownerChanged here - we'll do this when computing the new layout"