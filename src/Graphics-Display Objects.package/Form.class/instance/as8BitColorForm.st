converting
as8BitColorForm
	"Simple conversion of zero pixels to transparent.  Force it to 8 bits."

	| f map |
	f := ColorForm extent: self extent depth: 8.
	self displayOn: f at: self offset negated.
	map := Color indexedColors copy.
	map at: 1 put: Color transparent.
	f colors: map.
	f offset: self offset.
	^ f
