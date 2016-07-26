converting
elementsForwardIdentityTo: otherArray copyHash: copyHash
	"This primitive performs a bulk mutation, causing all pointers to the elements of the
	 receiver to be replaced by pointers to the corresponding elements of otherArray.
	 If copyHash is true, the identityHashes remain with the pointers rather than with the
	 objects so that the objects in the receiver should still be properly indexed in any
	 existing hashed structures after the mutation.  If copyHash is false, then the hashes
	 of the objects in otherArray remain unchanged.  If you know what you're doing this
	 may indeed be what you want."
	<primitive: 249 error: ec>
	self primitiveFailed