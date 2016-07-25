comparing
= aFreeTypeCacheEntry
	"equailty based on font,charcode, type, object, but not nextLink"
	
	(aFreeTypeCacheEntry isKindOf: FreeTypeCacheEntry) 
		ifFalse:[^false].
	^font = aFreeTypeCacheEntry font and: [
		charCode = aFreeTypeCacheEntry charCode
			and: [type = aFreeTypeCacheEntry type
				and:[object = aFreeTypeCacheEntry object]]]