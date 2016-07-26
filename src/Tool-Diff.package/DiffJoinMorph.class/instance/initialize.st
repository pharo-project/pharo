initialization
initialize
	"Initialize the receiver."

	super initialize.
	self
		mappings: OrderedCollection new;
		srcOffset: 0@0;
		dstOffset: 0@0