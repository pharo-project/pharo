removing
remove: oldObject 
	"Remove oldObject from the receiver's elements. Answer oldObject 
	unless no element is equal to oldObject, in which case, raise an error.
	ArrayedCollections cannot respond to this message."

	^ self remove: oldObject ifAbsent: [self errorNotFound: oldObject]