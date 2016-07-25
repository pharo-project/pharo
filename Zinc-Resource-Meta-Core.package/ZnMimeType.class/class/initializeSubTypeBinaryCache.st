private
initializeSubTypeBinaryCache
	"This private cache holds sub types of mime types that were classified #isBinary.
	In the future use a limited LRUCache when we no longer have to support Pharo 2.0"
	
	SubTypeBinaryCache := Dictionary new