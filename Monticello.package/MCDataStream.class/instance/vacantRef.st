other
vacantRef
	"Answer the magic 32-bit constant we use ***ON DISK*** as a stream 'reference
	 position' to identify a reference that's not yet filled in. This must be a
	 value that won't be used as an ordinary reference. Cf. outputReference: and
	 readReference. -- 
	 NOTE: We could use a different type ID for vacant-refs rather than writing
		object-references with a magic value. (The type ID and value are
		overwritten by ordinary object-references when weak refs are fullfilled.)"

	^ SmallInteger maxVal