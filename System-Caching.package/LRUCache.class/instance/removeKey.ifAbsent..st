removing
removeKey: key ifAbsent: block
	"If I currently cache key, remove the entry.
	Execute block when key is currently absent.
	Return the removed value."
	
	^ self critical: [
		(self includesKey: key)
			ifTrue: [ | link value |
				link := keyIndex removeKey: key.
				lruList removeLink: link.
				value := link value value.
				weight remove: value.
				value ]
			ifFalse: block ]