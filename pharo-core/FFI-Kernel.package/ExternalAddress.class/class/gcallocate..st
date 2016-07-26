instance creation
gcallocate: byteSize
	"Primitive. Allocate an object on the external heap.
	The external memory will be freed when i am garbage collected.
	BEWARE: there should be no copy of self, nor any pointer to a sub part..."
	
	| externalAddress |
	externalAddress := self allocate: byteSize.
	self finalizationRegistry add: externalAddress.
	^externalAddress